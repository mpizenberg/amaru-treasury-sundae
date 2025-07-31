module Page.Treasury exposing (Action, Model, Msg, UpdateContext, ViewContext, init, update, updateWithTx, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map
import Cardano.Address as Address exposing (CredentialHash, NetworkId)
import Cardano.Cip30 as Cip30
import Cardano.Script as Script exposing (NativeScript(..), PlutusScript)
import Cardano.Transaction as Transaction
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent as TxIntent exposing (TxFinalized)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Cmd.Extra
import Dict.Any
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Json.Decode as JD
import List.Extra
import MultisigScript
import Page.Loading exposing (Loaded, viewRootUtxo)
import Page.SignTx as SignTx
import Platform.Cmd as Cmd
import Route
import Task
import Time exposing (Posix)
import Treasury.Disburse as Disburse
import Treasury.LoadingParams as LoadingParams
import Treasury.Merge as Merge
import Treasury.Scope as Scope exposing (Scope, Scripts, StartDisburseInfo, viewDetailedUtxo)
import Treasury.Scopes as Scopes
import Treasury.Swap as Swap


type alias Model =
    { loaded : Loaded
    , action : Action
    , scripts : Scripts
    , error : Maybe String
    }


init : Scripts -> Loaded -> Model
init scripts loaded =
    { loaded = loaded
    , action = Idle
    , scripts = scripts
    , error = Nothing
    }


type Action
    = Idle
    | MergeTreasuryUtxos ScopeCtx ActionStatus
    | Disburse ScopeCtx ActionStatus Disburse.Form
    | Swap ScopeCtx ActionStatus Swap.Form


type alias ScopeCtx =
    { scopeName : String
    , scope : Scope
    , rootUtxo : OutputReference
    }


type ActionStatus
    = PreparingTx
    | BuildingFailure String
    | ReadyForSignature (Bytes TransactionId) TxFinalized



-- UPDATE ############################################################


type Msg
    = PublishScript (Bytes CredentialHash) PlutusScript
    | TreasuryMergingMsg TreasuryMergingMsg
    | TreasuryDisburseMsg TreasuryDisburseMsg
    | TreasurySwapMsg TreasurySwapMsg


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


type TreasurySwapMsg
    = StartSwap StartDisburseInfo
    | SwapFormMsg Swap.Msg
    | BuildSwapTransaction
    | BuildSwapTransactionWithTime Posix
    | CancelSwapAction


type alias UpdateContext a msg =
    { a
        | toMsg : Msg -> msg
        , routingConfig : Route.Config msg
        , db : JD.Value
        , connectedWallet : Maybe Cip30.Wallet
        , localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
    }


update : UpdateContext a msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case msg of
        TreasuryMergingMsg submsg ->
            handleTreasuryMergingMsg ctx submsg model

        TreasuryDisburseMsg submsg ->
            handleTreasuryDisburseMsg ctx submsg model

        TreasurySwapMsg submsg ->
            handleTreasurySwapMsg ctx submsg model

        PublishScript hash script ->
            createPublishScriptTx ctx hash script model



-- Merge


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
            ( handleBuildMergeTransaction ctx requiredSigners currentTime model, Cmd.none )

        CancelMergeAction ->
            ( { model | action = Idle }, Cmd.none )


handleStartMergeUtxos : String -> Scope -> OutputReference -> Model -> Model
handleStartMergeUtxos scopeName scope rootUtxo model =
    if Dict.Any.size scope.treasuryUtxos > 1 then
        let
            scopeCtx =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                }
        in
        { model | action = MergeTreasuryUtxos scopeCtx PreparingTx }

    else
        { model | error = Just "Scope has less than 2 UTxOs, so there is nothing to merge." }


handleBuildMergeTransaction : UpdateContext a msg -> List (Bytes CredentialHash) -> Posix -> Model -> Model
handleBuildMergeTransaction ctx requiredSigners currentTime ({ action } as model) =
    case ( action, ctx.connectedWallet ) of
        ( _, Nothing ) ->
            { model | error = Just "Please connect a wallet to build the Tx" }

        ( MergeTreasuryUtxos ({ scope, rootUtxo } as scopeCtx) _, Just wallet ) ->
            let
                buildCtx =
                    { localStateUtxos = ctx.localStateUtxos
                    , networkId = ctx.networkId
                    , wallet = wallet
                    }

                mergeUpdate newStatus =
                    { model | action = MergeTreasuryUtxos scopeCtx newStatus, error = Nothing }
            in
            case Merge.buildTx buildCtx rootUtxo currentTime scope requiredSigners of
                Ok ({ tx } as txFinalized) ->
                    mergeUpdate <|
                        ReadyForSignature (Transaction.computeTxId tx) txFinalized

                Err err ->
                    mergeUpdate <|
                        BuildingFailure (TxIntent.errorToString err)

        _ ->
            model



-- Disburse


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
                        \({ rootUtxo, scope } as scopeCtx) _ form ->
                            case Disburse.buildTx buildCtx rootUtxo currentTime scope form of
                                Ok ({ tx } as txFinalized) ->
                                    Disburse scopeCtx (ReadyForSignature (Transaction.computeTxId tx) txFinalized) form

                                Err err ->
                                    Disburse scopeCtx (BuildingFailure err) form
                    , Cmd.none
                    )

        CancelDisburseAction ->
            ( { model | action = Idle }, Cmd.none )


handleStartDisburse : StartDisburseInfo -> Model -> Model
handleStartDisburse { scopeName, scope, allOwners, rootUtxo, spendingUtxo } model =
    if not (Dict.Any.isEmpty scope.treasuryUtxos) then
        let
            scopeCtx =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                }

            form =
                Disburse.initForm scopeName allOwners spendingUtxo
        in
        { model | action = Disburse scopeCtx PreparingTx form, error = Nothing }

    else
        { model | error = Just <| scopeName ++ " treasury is empty. No disbursement possible." }


handleDisburseFormUpdate : (Disburse.Form -> Disburse.Form) -> Model -> Model
handleDisburseFormUpdate formUpdate model =
    handleDisburseUpdate model (\scopeCtx status form -> Disburse scopeCtx status <| formUpdate form)


handleDisburseUpdate : Model -> (ScopeCtx -> ActionStatus -> Disburse.Form -> Action) -> Model
handleDisburseUpdate model disburseUpdate =
    case model.action of
        Disburse scopeCtx status form ->
            { model | action = disburseUpdate scopeCtx status form, error = Nothing }

        _ ->
            model



-- Swap


handleTreasurySwapMsg : UpdateContext a msg -> TreasurySwapMsg -> Model -> ( Model, Cmd msg )
handleTreasurySwapMsg ctx msg model =
    case msg of
        StartSwap startSwapInfo ->
            ( handleStartSwap startSwapInfo model, Cmd.none )

        SwapFormMsg subMsg ->
            ( handleSwapFormUpdate (Swap.update subMsg) model, Cmd.none )

        BuildSwapTransaction ->
            ( model
            , Task.perform (ctx.toMsg << TreasurySwapMsg << BuildSwapTransactionWithTime) Time.now
            )

        BuildSwapTransactionWithTime currentTime ->
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
                    ( handleSwapUpdate model <|
                        \({ rootUtxo, scope } as scopeCtx) _ form ->
                            case Swap.validateForm ctx.networkId form of
                                Ok swapConfig ->
                                    case Swap.buildTx buildCtx rootUtxo currentTime scope swapConfig of
                                        Ok ({ tx } as txFinalized) ->
                                            Swap scopeCtx (ReadyForSignature (Transaction.computeTxId tx) txFinalized) form

                                        Err err ->
                                            Swap scopeCtx (BuildingFailure err) form

                                Err err ->
                                    Swap scopeCtx PreparingTx { form | error = Just err }
                    , Cmd.none
                    )

        CancelSwapAction ->
            ( { model | action = Idle }, Cmd.none )


handleStartSwap : StartDisburseInfo -> Model -> Model
handleStartSwap { scopeName, scope, allOwners, rootUtxo, spendingUtxo } model =
    if not (Dict.Any.isEmpty scope.treasuryUtxos) then
        let
            scopeCtx =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                }

            form =
                Swap.initForm scopeName allOwners spendingUtxo
        in
        { model | action = Swap scopeCtx PreparingTx form, error = Nothing }

    else
        { model | error = Just <| scopeName ++ " treasury is empty. No disbursement possible." }


handleSwapFormUpdate : (Swap.Form -> Swap.Form) -> Model -> Model
handleSwapFormUpdate formUpdate model =
    handleSwapUpdate model (\scopeCtx status form -> Swap scopeCtx status <| formUpdate form)


handleSwapUpdate : Model -> (ScopeCtx -> ActionStatus -> Swap.Form -> Action) -> Model
handleSwapUpdate model swapUpdate =
    case model.action of
        Swap scopeCtx status form ->
            { model | action = swapUpdate scopeCtx status form, error = Nothing }

        _ ->
            model



-- Publish


createPublishScriptTx : UpdateContext a msg -> Bytes CredentialHash -> PlutusScript -> Model -> ( Model, Cmd msg )
createPublishScriptTx ctx _ script model =
    case ctx.connectedWallet of
        Nothing ->
            ( { model | error = Just "Please connect your wallet to be able to create a Tx" }, Cmd.none )

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
                    ( { model | error = Just <| TxIntent.errorToString err }, Cmd.none )

                Ok { tx, expectedSignatures } ->
                    let
                        expectedSigners =
                            List.map (\keyHash -> { keyHash = keyHash, keyName = "Unidentified signer" }) expectedSignatures

                        signingRoute =
                            { tx = Just tx, expectedSigners = expectedSigners }
                    in
                    ( model, Cmd.Extra.perform <| ctx.routingConfig.urlChangedMsg <| Route.Signing signingRoute )



--


updateWithTx : Bytes TransactionId -> Model -> Model
updateWithTx txId ({ action } as model) =
    -- TODO: Detect all changes to the loaded treasury happening in the Tx
    { model | action = updateActionWithTx txId action }


updateActionWithTx : Bytes TransactionId -> Action -> Action
updateActionWithTx txId action =
    case action of
        MergeTreasuryUtxos _ (ReadyForSignature txIdWait _) ->
            if txIdWait == txId then
                Idle

            else
                action

        Disburse _ (ReadyForSignature txIdWait _) _ ->
            if txIdWait == txId then
                Idle

            else
                action

        _ ->
            action



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | toMsg : Msg -> msg
        , refreshUtxos : Loaded -> msg
        , routeConfig : Route.Config msg
        , networkId : NetworkId
        , connectedWallet : Maybe Cip30.Wallet
    }


view : ViewContext a msg -> Model -> Html msg
view ctx model =
    case model.action of
        Idle ->
            viewTreasurySection ctx model.loaded

        MergeTreasuryUtxos scopeCtx status ->
            viewMergeUtxosAction ctx scopeCtx status

        Disburse scopeCtx status form ->
            viewDisburseAction ctx scopeCtx status form

        Swap scopeCtx status form ->
            viewSwapAction ctx scopeCtx status form


viewTreasurySection : ViewContext a msg -> Loaded -> Html msg
viewTreasurySection ctx ({ rootUtxo, loadingParams, scopes, contingency } as loaded) =
    let
        allOwners =
            Scopes.map .owner scopes
                |> Scopes.toList
                |> List.Extra.zip [ "ledger", "consensus", "mercenaries", "marketing" ]

        scopeCtx =
            { networkId = ctx.networkId
            , rootUtxo = Tuple.first rootUtxo
            , allOwners = allOwners
            , publishScript = \hash script -> ctx.toMsg <| PublishScript hash script
            , refreshTreasuryUtxos = ctx.refreshUtxos loaded
            , startMergingUtxos = \name scope ref -> ctx.toMsg <| TreasuryMergingMsg <| StartMergeUtxos name scope ref
            , startDisburse = ctx.toMsg << TreasuryDisburseMsg << StartDisburse
            , startSwap = ctx.toMsg << TreasurySwapMsg << StartSwap
            }
    in
    div []
        [ Html.p [] [ text "Treasury fully loaded" ]
        , LoadingParams.viewReload loadingParams
        , viewRootUtxo rootUtxo
        , Scope.view scopeCtx "ledger" scopes.ledger
        , Scope.view scopeCtx "consensus" scopes.consensus
        , Scope.view scopeCtx "mercenaries" scopes.mercenaries
        , Scope.view scopeCtx "marketing" scopes.marketing
        , Scope.view scopeCtx "contingency" contingency
        ]



-- Merge UTxOs


viewMergeUtxosAction : ViewContext a msg -> ScopeCtx -> ActionStatus -> Html msg
viewMergeUtxosAction { toMsg, routeConfig, connectedWallet } { scopeName, scope } status =
    div []
        [ Html.h3 [] [ text ("Merge UTXOs - " ++ scopeName ++ " Scope") ]
        , div [] [ text ("UTXOs to merge: " ++ String.fromInt (Dict.Any.size scope.treasuryUtxos)) ]
        , Html.ul [] <|
            List.map (Html.li [] << viewDetailedUtxo) (Dict.Any.toList scope.treasuryUtxos)
        , case status of
            PreparingTx ->
                let
                    -- FIXME: temporary, something more robust should be done,
                    -- and not inside the view, but this works as a MVP.
                    requiredSigners =
                        MultisigScript.extractRequiredSigners scope.owner
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

            ReadyForSignature txId { tx, expectedSignatures } ->
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
                        [ SignTx.signingLink routeConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryMergingMsg CancelMergeAction ] [ text "Cancel" ]
                    ]
        ]



-- Disburse


viewDisburseAction : ViewContext a msg -> ScopeCtx -> ActionStatus -> Disburse.Form -> Html msg
viewDisburseAction { toMsg, routeConfig, connectedWallet } { scopeName } status form =
    div []
        [ Html.h3 [] [ text ("Disburse - " ++ scopeName ++ " Scope") ]
        , case status of
            PreparingTx ->
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
                    Disburse.viewForm disburseCtx scopeName form

            BuildingFailure error ->
                Html.map (toMsg << TreasuryDisburseMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelDisburseAction ] [ text "Cancel" ]
                        ]

            ReadyForSignature txId { tx, expectedSignatures } ->
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
                        [ SignTx.signingLink routeConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryDisburseMsg CancelDisburseAction ] [ text "Cancel" ]
                    ]
        ]



-- Swap


viewSwapAction : ViewContext a msg -> ScopeCtx -> ActionStatus -> Swap.Form -> Html msg
viewSwapAction { toMsg, routeConfig, connectedWallet, networkId } { scopeName } status form =
    div []
        [ Html.h3 [] [ text ("Swap - " ++ scopeName ++ " Scope") ]
        , case status of
            PreparingTx ->
                let
                    swapCtx =
                        { connectedWallet = connectedWallet
                        , networkId = networkId
                        , routeConfig = routeConfig
                        , msg =
                            { from = SwapFormMsg
                            , cancel = CancelSwapAction
                            , buildTx = BuildSwapTransaction
                            }
                        }
                in
                Html.map (toMsg << TreasurySwapMsg) <|
                    Swap.viewForm swapCtx scopeName form

            BuildingFailure error ->
                Html.map (toMsg << TreasurySwapMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelSwapAction ] [ text "Cancel" ]
                        ]

            ReadyForSignature txId { tx, expectedSignatures } ->
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
                        [ SignTx.signingLink routeConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasurySwapMsg CancelSwapAction ] [ text "Cancel" ]
                    ]
        ]
