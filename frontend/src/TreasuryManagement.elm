module TreasuryManagement exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Script as Script exposing (NativeScript(..), PlutusScript, PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Cardano.Witness as Witness
import Cmd.Extra
import ConcurrentTask exposing (ConcurrentTask)
import Dict.Any
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Json.Decode as JD
import List.Extra
import MultisigScript
import Natural as N exposing (Natural)
import Page.SignTx as SignTx
import Platform.Cmd as Cmd
import Route
import Task
import Time exposing (Posix)
import Treasury
import TreasuryManagement.Disburse as Disburse
import TreasuryManagement.Loading as Loading exposing (LoadedTreasury, LoadingTreasury, TaskCompleted, viewRootUtxo)
import TreasuryManagement.LoadingParams as LoadingParams
import TreasuryManagement.Scope as Scope exposing (Scope, Scripts, StartDisburseInfo, viewDetailedUtxo)
import TreasuryManagement.Scopes as Scopes
import TreasuryManagement.Setup exposing (SetupTxs)
import TreasuryManagement.SetupForm as SetupForm exposing (SetupForm)
import Utils exposing (spinner, viewError)


type alias Model =
    { treasuryLoadingParamsForm : LoadingParams.Form
    , treasuryManagement : TreasuryManagement
    , treasuryAction : TreasuryAction
    , scripts : Scripts
    , error : Maybe String
    }


init : Scripts -> LoadingParams.Form -> Model
init scripts treasuryLoadingParamsForm =
    { treasuryLoadingParamsForm = treasuryLoadingParamsForm
    , treasuryManagement = TreasuryUnspecified
    , treasuryAction = NoTreasuryAction
    , scripts = scripts
    , error = Nothing
    }


setLoadingParamsForm : LoadingParams.Form -> Model -> Model
setLoadingParamsForm treasuryLoadingParamsForm model =
    { model | treasuryLoadingParamsForm = treasuryLoadingParamsForm }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasurySetupForm SetupForm
    | TreasurySetupTxs SetupTxsState
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


type alias SetupTxsState =
    { txs : SetupTxs
    , treasury : LoadedTreasury
    , tracking :
        { scopes : TxState
        , permissions : TxState
        , registries : TxState
        }
    }


initSetupTxsState : SetupTxs -> LoadedTreasury -> SetupTxsState
initSetupTxsState txs treasury =
    { txs = txs
    , treasury = treasury
    , tracking =
        { scopes = TxNotSubmittedYet
        , permissions = TxNotSubmittedYet
        , registries = TxNotSubmittedYet
        }
    }


type TxState
    = TxNotSubmittedYet
    | TxSubmitting
    | TxSubmitted


type TreasuryAction
    = NoTreasuryAction
    | MergeTreasuryUtxos ScopeActionState
    | Disburse ScopeActionState Disburse.Form


type alias ScopeActionState =
    { scopeName : String
    , scope : Scope
    , rootUtxo : OutputReference
    , status : ActionStatus
    }


type ActionStatus
    = ActionIdle
    | BuildingFailure String
    | AwaitingSignature (Bytes TransactionId) TxFinalized


{-| Merge all UTxOs from a given scope.
-}
mergeUtxos : NetworkId -> OutputReference -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> ( List TxIntent, List TxOtherInfo )
mergeUtxos networkId rootUtxo scope requiredSigners validityRange =
    let
        utxos =
            Dict.Any.toList scope.treasuryUtxos

        permissionsWitnessSource =
            case scope.permissionsScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes permissionsScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        requiredWithdrawals =
            -- Withdrawal with the scope owner script
            [ { stakeCredential =
                    { networkId = networkId
                    , stakeCredential = ScriptHash permissionsScriptHash
                    }
              , amount = N.zero
              , scriptWitness =
                    Just <|
                        Witness.Plutus
                            { script =
                                ( Script.plutusVersion permissionsScript
                                , permissionsWitnessSource
                                )
                            , redeemerData = \_ -> Data.Constr N.zero []
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ( permissionsScriptHash, permissionsScript ) =
            scope.permissionsScript

        ( treasuryScriptHash, treasuryScript ) =
            scope.sundaeTreasuryScript

        treasuryWitnessSource =
            case scope.sundaeTreasuryScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes treasuryScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        -- The address must have a stake cred (delegated to always abstain)
        scopeTreasuryAddress =
            Address.base networkId (Address.ScriptHash treasuryScriptHash) (Address.ScriptHash treasuryScriptHash)
    in
    Treasury.reorganize
        { scriptWitnessSource = treasuryWitnessSource
        , registryOutputRef = Tuple.first scope.registryUtxo
        , additionalOutputRefs = [ rootUtxo ]
        , requiredSigners = requiredSigners
        , validityRange = validityRange
        , requiredWithdrawals = requiredWithdrawals
        , spentUtxos = utxos
        , receivers = \value -> [ TxIntent.SendTo scopeTreasuryAddress value ]
        }



-- UPDATE ############################################################


type Msg
    = StartTreasurySetup
    | StartTreasurySetupWithCurrentTime Posix
    | UpdateSetupForm SetupForm.Msg
    | TryBuildSetupTxs
    | TreasuryLoadingParamsMsg LoadingParams.Msg
    | StartTreasuryLoading
    | RefreshTreasuryUtxos
    | TreasuryMergingMsg TreasuryMergingMsg
    | TreasuryDisburseMsg TreasuryDisburseMsg
      -- Publish
    | PublishScript (Bytes CredentialHash) PlutusScript


type TreasuryMergingMsg
    = StartMergeUtxos String Scope OutputReference
    | BuildMergeTransaction (List (Bytes CredentialHash))
    | BuildMergeTransactionWithTime (List (Bytes CredentialHash)) Posix
    | CancelMergeAction


type TreasuryDisburseMsg
    = StartDisburse StartDisburseInfo
    | DisburseFormMsg Disburse.Msg
    | BuildDisburseTransaction
    | BuildDisburseTransactionWithTime Posix
    | CancelDisburseAction


type alias UpdateContext a msg =
    { a
        | toMsg : Msg -> msg
        , routingConfig : Route.Config msg
        , db : JD.Value
        , connectedWallet : Maybe Cip30.Wallet
        , localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
    }


type alias OutMsg =
    { updatedLocalState : Utxo.RefDict Output
    , runTasks : List (ConcurrentTask String TaskCompleted)
    }


update : UpdateContext a msg -> Msg -> Model -> ( Model, Cmd msg, OutMsg )
update ctx msg ({ treasuryLoadingParamsForm } as model) =
    let
        noOutMsg =
            { updatedLocalState = ctx.localStateUtxos
            , runTasks = []
            }
    in
    case msg of
        StartTreasurySetup ->
            ( model
            , Task.perform (ctx.toMsg << StartTreasurySetupWithCurrentTime) Time.now
            , noOutMsg
            )

        StartTreasurySetupWithCurrentTime currentTime ->
            ( { model | treasuryManagement = TreasurySetupForm <| SetupForm.init currentTime }, Cmd.none, noOutMsg )

        UpdateSetupForm subMsg ->
            ( handleTreasurySetupFormUpdate subMsg model, Cmd.none, noOutMsg )

        TryBuildSetupTxs ->
            case model.treasuryManagement of
                TreasurySetupForm form ->
                    ( { model | treasuryManagement = handleBuildSetupTxs ctx model.scripts form }, Cmd.none, noOutMsg )

                _ ->
                    ( model, Cmd.none, noOutMsg )

        TreasuryLoadingParamsMsg paramsMsg ->
            ( { model | treasuryLoadingParamsForm = LoadingParams.updateForm paramsMsg treasuryLoadingParamsForm }, Cmd.none, noOutMsg )

        StartTreasuryLoading ->
            case Loading.startTreasuryLoading ctx model.scripts treasuryLoadingParamsForm of
                Ok ( loadingTreasury, outMsg ) ->
                    ( { model | treasuryManagement = TreasuryLoading loadingTreasury, error = Nothing }
                    , Cmd.none
                    , outMsg
                    )

                Err error ->
                    ( { model | treasuryLoadingParamsForm = { treasuryLoadingParamsForm | error = Just error } }, Cmd.none, noOutMsg )

        RefreshTreasuryUtxos ->
            case model.treasuryManagement of
                TreasuryFullyLoaded loadedTreasury ->
                    let
                        ( loadingTreasury, outMsg ) =
                            Loading.refreshTreasuryUtxos ctx loadedTreasury
                    in
                    ( { model | treasuryManagement = TreasuryLoading loadingTreasury }, Cmd.none, outMsg )

                _ ->
                    ( model, Cmd.none, noOutMsg )

        TreasuryMergingMsg submsg ->
            handleTreasuryMergingMsg ctx submsg model
                |> (\updated -> ( Tuple.first updated, Tuple.second updated, noOutMsg ))

        TreasuryDisburseMsg submsg ->
            handleTreasuryDisburseMsg ctx submsg model
                |> (\updated -> ( Tuple.first updated, Tuple.second updated, noOutMsg ))

        PublishScript hash script ->
            createPublishScriptTx ctx hash script model


handleTreasurySetupFormUpdate : SetupForm.Msg -> Model -> Model
handleTreasurySetupFormUpdate msg model =
    case model.treasuryManagement of
        TreasurySetupForm form ->
            { model | treasuryManagement = TreasurySetupForm <| SetupForm.update msg form }

        _ ->
            model


handleBuildSetupTxs : UpdateContext a msg -> Scripts -> SetupForm -> TreasuryManagement
handleBuildSetupTxs ctx scripts form =
    case form.validation of
        Just (Ok scopeOwners) ->
            case ctx.connectedWallet of
                Nothing ->
                    TreasurySetupForm { form | validation = Just <| Err "Please connect wallet first" }

                Just wallet ->
                    case TreasuryManagement.Setup.setupAmaruTreasury ctx.localStateUtxos scripts ctx.networkId wallet scopeOwners of
                        Err error ->
                            TreasurySetupForm { form | validation = Just <| Err error }

                        Ok ( txs, treasury ) ->
                            TreasurySetupTxs <| initSetupTxsState txs treasury

        _ ->
            TreasurySetupForm form


handleTreasuryMergingMsg : UpdateContext a msg -> TreasuryMergingMsg -> Model -> ( Model, Cmd msg )
handleTreasuryMergingMsg ctx msg model =
    case msg of
        StartMergeUtxos scopeName scope rootUtxo ->
            ( handleStartMergeUtxos scopeName scope rootUtxo model, Cmd.none )

        BuildMergeTransaction requiredSigners ->
            ( model
            , Task.perform (ctx.toMsg << TreasuryMergingMsg << BuildMergeTransactionWithTime requiredSigners) Time.now
            )

        BuildMergeTransactionWithTime requiredSigners currentTime ->
            ( handleBuildMergeTransaction ctx model requiredSigners currentTime, Cmd.none )

        CancelMergeAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none )


handleStartMergeUtxos : String -> Scope -> OutputReference -> Model -> Model
handleStartMergeUtxos scopeName scope rootUtxo model =
    if Dict.Any.size scope.treasuryUtxos > 1 then
        let
            mergeState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = ActionIdle
                }
        in
        { model | treasuryAction = MergeTreasuryUtxos mergeState }

    else
        { model | error = Just "Scope has less than 2 UTxOs, so there is nothing to merge." }


handleBuildMergeTransaction : UpdateContext a msg -> Model -> List (Bytes CredentialHash) -> Posix -> Model
handleBuildMergeTransaction ctx model requiredSigners currentTime =
    case ( model.treasuryAction, ctx.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                slotConfig =
                    case ctx.networkId of
                        Mainnet ->
                            Uplc.slotConfigMainnet

                        Testnet ->
                            Uplc.slotConfigPreview

                slot60sAgo =
                    (Time.posixToMillis currentTime - 1000 * 60)
                        |> Time.millisToPosix
                        |> Uplc.timeToSlot slotConfig

                slotIn6Hours =
                    (Time.posixToMillis currentTime + 1000 * 3600 * 6)
                        |> Time.millisToPosix
                        |> Uplc.timeToSlot slotConfig

                validityRange =
                    Just { start = N.toInt slot60sAgo, end = slotIn6Hours }

                ( mergeTxIntents, mergeOtherInfo ) =
                    mergeUtxos ctx.networkId mergeState.rootUtxo mergeState.scope requiredSigners validityRange

                feeSource =
                    Cip30.walletChangeAddress wallet

                txResult =
                    TxIntent.finalizeAdvanced
                        { govState = TxIntent.emptyGovernanceState
                        , localStateUtxos = ctx.localStateUtxos
                        , coinSelectionAlgo = CoinSelection.largestFirst
                        , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource mergeTxIntents
                        , costModels = Uplc.conwayDefaultCostModels
                        }
                        (TxIntent.AutoFee { paymentSource = feeSource })
                        mergeOtherInfo
                        mergeTxIntents

                updatedMergeState =
                    case txResult of
                        Ok ({ tx } as txFinalized) ->
                            { mergeState | status = AwaitingSignature (Transaction.computeTxId tx) txFinalized }

                        Err err ->
                            { mergeState | status = BuildingFailure <| TxIntent.errorToString err }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }
            in
            updatedModel

        _ ->
            model



--


handleTreasuryDisburseMsg : UpdateContext a msg -> TreasuryDisburseMsg -> Model -> ( Model, Cmd msg )
handleTreasuryDisburseMsg ctx msg model =
    case msg of
        StartDisburse startDisburseInfo ->
            ( handleStartDisburse startDisburseInfo model, Cmd.none )

        DisburseFormMsg subMsg ->
            ( handleDisburseFormUpdate (Disburse.update subMsg) model, Cmd.none )

        BuildDisburseTransaction ->
            ( model
            , Task.perform (ctx.toMsg << TreasuryDisburseMsg << BuildDisburseTransactionWithTime) Time.now
            )

        BuildDisburseTransactionWithTime currentTime ->
            case ctx.connectedWallet of
                Nothing ->
                    ( { model | error = Just "Please connect your wallet" }, Cmd.none )

                Just wallet ->
                    let
                        buildCtx =
                            { localStateUtxos = ctx.localStateUtxos
                            , networkId = ctx.networkId
                            , wallet = wallet
                            }
                    in
                    ( handleDisburseUpdate model <|
                        \action form ->
                            case Disburse.buildTx buildCtx action.rootUtxo currentTime action.scope form of
                                Ok ({ tx } as txFinalized) ->
                                    Disburse { action | status = AwaitingSignature (Transaction.computeTxId tx) txFinalized } form

                                Err err ->
                                    Disburse { action | status = BuildingFailure err } form
                    , Cmd.none
                    )

        CancelDisburseAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none )


handleStartDisburse : StartDisburseInfo -> Model -> Model
handleStartDisburse { scopeName, scope, allOwners, rootUtxo, spendingUtxo } model =
    if not (Dict.Any.isEmpty scope.treasuryUtxos) then
        let
            actionState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = ActionIdle
                }

            form =
                Disburse.initForm scopeName allOwners spendingUtxo
        in
        { model | treasuryAction = Disburse actionState form, error = Nothing }

    else
        { model | error = Just <| scopeName ++ " treasury is empty. No disbursement possible." }


handleDisburseFormUpdate : (Disburse.Form -> Disburse.Form) -> Model -> Model
handleDisburseFormUpdate formUpdate model =
    handleDisburseUpdate model (\state form -> Disburse state <| formUpdate form)


handleDisburseUpdate : Model -> (ScopeActionState -> Disburse.Form -> TreasuryAction) -> Model
handleDisburseUpdate model disburseUpdate =
    case model.treasuryAction of
        Disburse disburseState form ->
            { model | treasuryAction = disburseUpdate disburseState form, error = Nothing }

        _ ->
            model


createPublishScriptTx : UpdateContext a msg -> Bytes CredentialHash -> PlutusScript -> Model -> ( Model, Cmd msg, OutMsg )
createPublishScriptTx ctx _ script model =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case ctx.connectedWallet of
        Nothing ->
            ( { model | error = Just "Please connect your wallet to be able to create a Tx" }, Cmd.none, noOutMsg )

        Just wallet ->
            let
                walletAddress =
                    Cip30.walletChangeAddress wallet

                walletKey =
                    Address.extractPubKeyHash walletAddress
                        |> Maybe.withDefault (Bytes.dummy 28 "")

                -- For now this is published in a multisig with just the wallet key as payment credential,
                -- and re-using the same stake credential to keep it staked.
                utxoAddress =
                    Address.script ctx.networkId (Script.hash <| Script.Native <| ScriptPubkey walletKey)
                        |> Address.setShelleyStakeCred (Address.extractStakeCredential walletAddress)

                scriptUtxo =
                    Utxo.withMinAda <|
                        { address = utxoAddress
                        , amount = Value.zero
                        , datumOption = Nothing
                        , referenceScript = Just <| Script.refFromScript <| Script.Plutus script
                        }

                publishIntents =
                    [ TxIntent.Spend <|
                        TxIntent.FromWallet
                            { address = walletAddress
                            , value = scriptUtxo.amount
                            , guaranteedUtxos = []
                            }
                    , TxIntent.SendToOutput scriptUtxo
                    ]

                txResult =
                    TxIntent.finalize ctx.localStateUtxos [] publishIntents
            in
            case txResult of
                Err err ->
                    ( { model | error = Just <| TxIntent.errorToString err }, Cmd.none, noOutMsg )

                Ok { tx, expectedSignatures } ->
                    let
                        expectedSigners =
                            List.map (\keyHash -> { keyHash = keyHash, keyName = "Unidentified signer" }) expectedSignatures

                        signingRoute =
                            { tx = Just tx, expectedSigners = expectedSigners }
                    in
                    ( model, Cmd.Extra.perform <| ctx.routingConfig.urlChangedMsg <| Route.Signing signingRoute, noOutMsg )



--


updateWithTx : Utxo.RefDict Output -> Maybe { a | txId : Bytes TransactionId, tx : Transaction } -> Model -> ( Model, OutMsg )
updateWithTx localStateUtxos maybeTx ({ treasuryManagement, treasuryAction } as model) =
    case maybeTx of
        Nothing ->
            ( model, OutMsg localStateUtxos [] )

        Just { txId, tx } ->
            let
                updatedLocalState =
                    (TxIntent.updateLocalState txId tx localStateUtxos).updatedState
            in
            case treasuryManagement of
                TreasurySetupTxs setupState ->
                    ( { model
                        | treasuryManagement =
                            markTxAsSubmitted txId setupState
                                |> upgradeToLoadedIfSetupIsDone
                        , treasuryAction = updateTreasuryActionWithTx txId treasuryAction
                      }
                    , OutMsg updatedLocalState []
                    )

                TreasuryFullyLoaded loadedTreasury ->
                    ( { model
                        | treasuryManagement =
                            updateTreasuryUtxos txId tx loadedTreasury
                                |> TreasuryFullyLoaded
                        , treasuryAction = updateTreasuryActionWithTx txId treasuryAction
                      }
                    , OutMsg updatedLocalState []
                    )

                -- No other management state should trigger a Tx.
                -- We just update the local state in case it’s a Tx unrelated to treasury management.
                _ ->
                    ( model, OutMsg updatedLocalState [] )


markTxAsSubmitted : Bytes TransactionId -> SetupTxsState -> SetupTxsState
markTxAsSubmitted txId ({ tracking } as state) =
    if txId == state.txs.scopes.txId then
        { state | tracking = { tracking | scopes = TxSubmitted } }

    else if txId == state.txs.permissions.txId then
        { state | tracking = { tracking | permissions = TxSubmitted } }

    else if txId == state.txs.registries.txId then
        { state | tracking = { tracking | registries = TxSubmitted } }

    else
        state


upgradeToLoadedIfSetupIsDone : SetupTxsState -> TreasuryManagement
upgradeToLoadedIfSetupIsDone ({ tracking } as state) =
    case ( tracking.scopes, tracking.permissions, tracking.registries ) of
        ( TxSubmitted, TxSubmitted, TxSubmitted ) ->
            TreasuryFullyLoaded state.treasury

        _ ->
            TreasurySetupTxs state


updateTreasuryUtxos : Bytes TransactionId -> Transaction -> LoadedTreasury -> LoadedTreasury
updateTreasuryUtxos txId tx loadedTreasury =
    -- TODO: Detect all changes to the treasury happening in the Tx
    loadedTreasury


updateTreasuryActionWithTx : Bytes TransactionId -> TreasuryAction -> TreasuryAction
updateTreasuryActionWithTx txId treasuryAction =
    let
        maybeTxIdWait =
            case treasuryAction of
                MergeTreasuryUtxos { status } ->
                    case status of
                        AwaitingSignature txIdWait _ ->
                            Just txIdWait

                        _ ->
                            Nothing

                Disburse { status } _ ->
                    case status of
                        AwaitingSignature txIdWait _ ->
                            Just txIdWait

                        _ ->
                            Nothing

                NoTreasuryAction ->
                    Nothing
    in
    case Maybe.map ((==) txId) maybeTxIdWait of
        Just True ->
            NoTreasuryAction

        _ ->
            treasuryAction



-- Tasks


handleCompletedLoadingTask : UpdateContext a msg -> Loading.TaskCompleted -> Model -> ( Model, OutMsg )
handleCompletedLoadingTask ctx task ({ treasuryLoadingParamsForm } as model) =
    case model.treasuryManagement of
        TreasuryLoading loadingTreasury ->
            let
                encounteredLoadingError error =
                    ( { model
                        | treasuryManagement = TreasuryUnspecified
                        , treasuryLoadingParamsForm = { treasuryLoadingParamsForm | error = Just error }
                      }
                    , OutMsg ctx.localStateUtxos []
                    )
            in
            case Loading.updateWithCompletedTask ctx model.scripts task loadingTreasury of
                Ok ( updatedLoadingTreasury, { updatedLocalState, runTasks } as outMsg ) ->
                    case Loading.upgradeIfTreasuryLoadingFinished updatedLocalState updatedLoadingTreasury of
                        Just ( loadedTreasury, loadedLocalState ) ->
                            case Loading.doubleCheckTreasuryScriptsHashes loadedTreasury of
                                Ok _ ->
                                    ( { model | treasuryManagement = TreasuryFullyLoaded loadedTreasury }, OutMsg loadedLocalState runTasks )

                                Err error ->
                                    encounteredLoadingError error

                        Nothing ->
                            ( { model | treasuryManagement = TreasuryLoading updatedLoadingTreasury }, outMsg )

                Err error ->
                    encounteredLoadingError error

        _ ->
            ( model, OutMsg ctx.localStateUtxos [] )



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | toMsg : Msg -> msg
        , routingConfig : Route.Config msg
        , networkId : NetworkId
        , connectedWallet : Maybe Cip30.Wallet
    }


view : ViewContext a msg -> Model -> Html msg
view ctx model =
    case model.treasuryAction of
        NoTreasuryAction ->
            viewTreasurySection ctx model.treasuryLoadingParamsForm model.treasuryManagement

        MergeTreasuryUtxos mergeState ->
            viewMergeUtxosAction ctx mergeState

        Disburse disburseState form ->
            viewDisburseAction ctx disburseState form


viewTreasurySection : ViewContext a msg -> LoadingParams.Form -> TreasuryManagement -> Html msg
viewTreasurySection ctx params treasuryManagement =
    case treasuryManagement of
        TreasuryUnspecified ->
            Html.map ctx.toMsg <|
                div []
                    [ Html.h2 [] [ Html.text "Setup a new treasury" ]
                    , Html.button [ onClick StartTreasurySetup ]
                        [ text "Setup a new treasury" ]
                    , Html.h2 [] [ Html.text "Load an existing treasury" ]
                    , Html.p [] [ text "With the following parameters:" ]
                    , Html.map TreasuryLoadingParamsMsg <|
                        LoadingParams.viewForm params
                    , Html.button [ onClick StartTreasuryLoading ]
                        [ text "Load treasury" ]
                    , viewError params.error
                    ]

        TreasurySetupForm form ->
            Html.map ctx.toMsg <|
                SetupForm.view { toMsg = UpdateSetupForm, tryBuildSetupTxs = TryBuildSetupTxs } form

        TreasurySetupTxs state ->
            viewSetupTxsState ctx state

        TreasuryLoading loadingTreasury ->
            Loading.viewLoading loadingTreasury

        TreasuryFullyLoaded { rootUtxo, loadingParams, scopes, contingency } ->
            let
                allOwners =
                    Scopes.map .owner scopes
                        |> Scopes.toList
                        |> List.Extra.zip [ "ledger", "consensus", "mercenaries", "marketing" ]

                scopeCtx =
                    { networkId = ctx.networkId
                    , rootUtxo = Tuple.first rootUtxo
                    , allOwners = allOwners
                    , publishScript = PublishScript
                    , refreshTreasuryUtxos = RefreshTreasuryUtxos
                    , startMergingUtxos = \name scope ref -> TreasuryMergingMsg <| StartMergeUtxos name scope ref
                    , startDisburse = TreasuryDisburseMsg << StartDisburse
                    }
            in
            div []
                [ Html.p [] [ text "Treasury fully loaded" ]
                , LoadingParams.viewReload loadingParams
                , viewRootUtxo rootUtxo
                , Html.map ctx.toMsg <| Scope.view scopeCtx "ledger" scopes.ledger
                , Html.map ctx.toMsg <| Scope.view scopeCtx "consensus" scopes.consensus
                , Html.map ctx.toMsg <| Scope.view scopeCtx "mercenaries" scopes.mercenaries
                , Html.map ctx.toMsg <| Scope.view scopeCtx "marketing" scopes.marketing
                , Html.map ctx.toMsg <| Scope.view scopeCtx "contingency" contingency
                ]


viewSetupTxsState : ViewContext a msg -> SetupTxsState -> Html msg
viewSetupTxsState ctx { txs, treasury, tracking } =
    div []
        [ Html.h2 [] [ text "Treasury State after Initialization" ]
        , viewRootUtxo treasury.rootUtxo
        , Scope.viewSetup "ledger" treasury.scopes.ledger
        , Scope.viewSetup "consensus" treasury.scopes.consensus
        , Scope.viewSetup "mercenaries" treasury.scopes.mercenaries
        , Scope.viewSetup "marketing" treasury.scopes.marketing
        , Scope.viewSetup "contingency" treasury.contingency
        , Html.h2 [] [ text "Txs to submit for Treasury Initialization" ]
        , Html.h3 [] [ text "Scope Owners Definition" ]
        , viewTxStatus ctx tracking.scopes txs.scopes
        , Html.h3 [] [ text "Permissions Stake Registration" ]
        , viewTxStatus ctx tracking.permissions txs.permissions
        , Html.h3 [] [ text "Registries Initializations" ]
        , viewTxStatus ctx tracking.registries txs.registries
        ]


viewTxStatus : ViewContext a msg -> TxState -> SignTx.Prep -> Html msg
viewTxStatus { routingConfig } txState ({ txId, tx, expectedSignatures, signerDescriptions } as prep) =
    let
        displaySigner signer =
            case Bytes.Map.get signer signerDescriptions of
                Nothing ->
                    Html.li [] [ text <| "Signer: " ++ Bytes.toHex signer ]

                Just description ->
                    Html.li [] [ text <| description ++ ": " ++ Bytes.toHex signer ]

        actions =
            case txState of
                TxNotSubmittedYet ->
                    Html.button []
                        [ SignTx.signingLink routingConfig prep [] [ text "Sign on signing page" ] ]

                TxSubmitting ->
                    div [] [ text "Tx is being submitted ... ", spinner ]

                TxSubmitted ->
                    text "Tx has been submitted!"
    in
    div []
        [ Html.p [] [ text <| "Tx state: " ++ Debug.toString txState ]
        , Html.p []
            [ text <| "Expected signers:"
            , Html.ul [] <| List.map displaySigner expectedSignatures
            ]
        , Html.p [] [ actions ]
        , Html.p [] [ text <| "Tx details:" ]
        , Html.p [] [ text <| "Tx ID: " ++ Bytes.toHex txId ]
        , Html.pre [] [ text <| prettyTx tx ]
        ]



-- Merge UTxOs


viewMergeUtxosAction : ViewContext a msg -> ScopeActionState -> Html msg
viewMergeUtxosAction { toMsg, routingConfig, connectedWallet } mergeState =
    div []
        [ Html.h3 [] [ text ("Merge UTXOs - " ++ mergeState.scopeName ++ " Scope") ]
        , div [] [ text ("UTXOs to merge: " ++ String.fromInt (Dict.Any.size mergeState.scope.treasuryUtxos)) ]
        , Html.ul [] <|
            List.map (Html.li [] << viewDetailedUtxo) (Dict.Any.toList mergeState.scope.treasuryUtxos)
        , case mergeState.status of
            ActionIdle ->
                let
                    -- FIXME: temporary, something more robust should be done,
                    -- and not inside the view, but this works as a MVP.
                    requiredSigners =
                        MultisigScript.extractRequiredSigners mergeState.scope.owner
                in
                Html.map (toMsg << TreasuryMergingMsg) <|
                    case connectedWallet of
                        Nothing ->
                            div []
                                [ Html.p [] [ text "Please connect your wallet first, to pay the Tx fees." ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                        Just _ ->
                            div []
                                [ Html.p [] [ text "Ready to build merge transaction. This will combine all UTXOs in this scope into a single UTXO." ]
                                , Html.button [ onClick (BuildMergeTransaction requiredSigners) ] [ text "Build Transaction" ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

            BuildingFailure error ->
                Html.map (toMsg << TreasuryMergingMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                        ]

            AwaitingSignature txId { tx, expectedSignatures } ->
                let
                    signingPrep =
                        { txId = txId
                        , tx = tx
                        , expectedSignatures = expectedSignatures

                        -- TODO: signerDescriptions : BytesMap CredentialHash String
                        , signerDescriptions = Bytes.Map.empty
                        }
                in
                div []
                    [ Html.p [] [ text "Transaction built successfully." ]
                    , Html.pre [] [ text <| prettyTx tx ]
                    , Html.button []
                        [ SignTx.signingLink routingConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryMergingMsg CancelMergeAction ] [ text "Cancel" ]
                    ]
        ]



-- Disburse


viewDisburseAction : ViewContext a msg -> ScopeActionState -> Disburse.Form -> Html msg
viewDisburseAction { toMsg, routingConfig, connectedWallet } actionState form =
    div []
        [ Html.h3 [] [ text ("Disburse - " ++ actionState.scopeName ++ " Scope") ]
        , case actionState.status of
            ActionIdle ->
                let
                    disburseCtx =
                        { connectedWallet = connectedWallet
                        , msg =
                            { from = DisburseFormMsg
                            , cancel = CancelDisburseAction
                            , buildTx = BuildDisburseTransaction
                            }
                        }
                in
                Html.map (toMsg << TreasuryDisburseMsg) <|
                    Disburse.viewForm disburseCtx actionState.scopeName form

            BuildingFailure error ->
                Html.map (toMsg << TreasuryDisburseMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelDisburseAction ] [ text "Cancel" ]
                        ]

            AwaitingSignature txId { tx, expectedSignatures } ->
                let
                    signingPrep =
                        { txId = txId
                        , tx = tx
                        , expectedSignatures = expectedSignatures

                        -- signerDescriptions : BytesMap CredentialHash String
                        , signerDescriptions = Bytes.Map.empty
                        }
                in
                div []
                    [ Html.p [] [ text "Transaction built successfully." ]
                    , Html.pre [] [ text <| prettyTx tx ]
                    , Html.button []
                        [ SignTx.signingLink routingConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryDisburseMsg CancelDisburseAction ] [ text "Cancel" ]
                    ]
        ]
