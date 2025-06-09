port module Main exposing (main)

import Api exposing (ProtocolParams)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.Transaction exposing (Transaction)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent as TxIntent exposing (TxFinalizationError, TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import ConcurrentTask exposing (ConcurrentTask)
import Dict
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData)
import Result.Extra
import Treasury exposing (SpendConfig)
import Types


type alias Flags =
    { url : String
    , db : JD.Value
    , pragmaScriptHash : String
    , registriesSeedUtxo : { transactionId : String, outputIndex : Int }
    , treasuryConfigExpiration : Int
    , blueprints : List JD.Value
    }


main : Program Flags Model Msg
main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ fromWallet WalletMsg

                    -- , onUrlChange (locationHrefToRoute >> UrlChanged)
                    , onUrlChange UrlChanged
                    , ConcurrentTask.onProgress
                        { send = sendTask
                        , receive = receiveTask
                        , onProgress = OnTaskProgress
                        }
                        model.taskPool
                    ]
        , view = view
        }


port toWallet : JD.Value -> Cmd msg


port fromWallet : (JD.Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port sendTask : JD.Value -> Cmd msg


port receiveTask : (JD.Value -> msg) -> Sub msg


type Msg
    = NoMsg
    | UrlChanged String
    | WalletMsg JD.Value
    | DisconnectWallet
    | ConnectButtonClicked { id : String }
    | GotNetworkParams (Result Http.Error ProtocolParams)
      -- Treasury management
    | TreasuryMergingMsg TreasuryMergingMsg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = LoadedPragmaUtxo ( OutputReference, Output )
    | LoadedRegistryUtxos (Scopes ( OutputReference, Output ))
    | LoadedTreasuriesUtxos (Scopes (Utxo.RefDict Output))



-- MODEL


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg String TaskCompleted
    , db : JD.Value
    , page : Page
    , networkId : NetworkId
    , protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , pragmaScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : OutputReference
    , treasuryConfigExpiration : Natural
    , scripts : Scripts
    , treasuryManagement : TreasuryManagement
    , treasuryAction : TreasuryAction
    , error : Maybe String
    }


initialModel : JD.Value -> String -> ( String, Int ) -> Int -> Scripts -> Model
initialModel db pragmaScriptHash ( txId, outputIndex ) expiration scripts =
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , pragmaScriptHash = Bytes.fromHexUnchecked pragmaScriptHash
    , registriesSeedUtxo = OutputReference (Bytes.fromHexUnchecked txId) outputIndex
    , treasuryConfigExpiration = N.fromSafeInt expiration
    , scripts = scripts
    , treasuryManagement = TreasuryUnspecified
    , treasuryAction = NoTreasuryAction
    , error = Nothing
    }


type Page
    = Home


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , registryTrap : PlutusScript
    , scopePermissions : PlutusScript
    }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


type alias LoadingTreasury =
    { rootUtxo : RemoteData String ( OutputReference, Output )
    , scopes : Scopes LoadingScope
    }


initLoadingTreasury : PlutusScript -> Bytes CredentialHash -> PlutusScript -> OutputReference -> Natural -> Result String LoadingTreasury
initLoadingTreasury unappliedScopePermissionScript pragmaScriptHash unappliedRegistryTrapScript registriesSeedUtxo treasuryConfigExpiration =
    let
        initLoadingScopeWithIndex index =
            initLoadingScope
                unappliedScopePermissionScript
                pragmaScriptHash
                unappliedRegistryTrapScript
                registriesSeedUtxo
                treasuryConfigExpiration
                index

        loadingTreasury ledger consensus mercenaries marketing =
            { rootUtxo = RemoteData.Loading
            , scopes = Scopes ledger consensus mercenaries marketing
            }
    in
    Result.map4 loadingTreasury
        (initLoadingScopeWithIndex 0)
        (initLoadingScopeWithIndex 1)
        (initLoadingScopeWithIndex 2)
        (initLoadingScopeWithIndex 3)


type alias LoadingScope =
    { owner : Maybe MultisigScript
    , permissionsScriptApplied : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptApplied : RemoteData String ( Bytes CredentialHash, PlutusScript )
    , registryNftPolicyId : Bytes PolicyId
    , registryUtxo : RemoteData String ( OutputReference, Output )
    , treasuryUtxos : RemoteData String (Utxo.RefDict Output)
    , expiration : Natural
    }


initLoadingScope : PlutusScript -> Bytes CredentialHash -> PlutusScript -> OutputReference -> Natural -> Int -> Result String LoadingScope
initLoadingScope unappliedScopePermissionScript pragmaScriptHash unappliedRegistryTrapScript registriesSeedUtxo treasuryConfigExpiration scopeIndex =
    let
        permissionsScriptResult =
            Uplc.applyParamsToScript
                [ Data.Bytes <| Bytes.toAny pragmaScriptHash
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedScopePermissionScript

        registryScriptResult =
            Uplc.applyParamsToScript
                [ Utxo.outputReferenceToData registriesSeedUtxo
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedRegistryTrapScript

        loadingScope permissionsScript registryScript =
            { owner = Nothing
            , permissionsScriptApplied = ( Script.hash <| Script.Plutus permissionsScript, permissionsScript )
            , sundaeTreasuryScriptApplied = RemoteData.Loading
            , registryNftPolicyId = Script.hash <| Script.Plutus registryScript
            , registryUtxo = RemoteData.Loading
            , treasuryUtxos = RemoteData.NotAsked
            , expiration = treasuryConfigExpiration
            }
    in
    Result.map2 loadingScope
        permissionsScriptResult
        registryScriptResult


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , scopes : Scopes Scope
    }


type alias Scopes a =
    { ledger : a
    , consensus : a
    , mercenaries : a
    , marketing : a
    }


type alias Scope =
    { owner : MultisigScript
    , permissionsScript : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScript : ( Bytes CredentialHash, PlutusScript )
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , treasuryUtxos : Utxo.RefDict Output
    }


type TreasuryAction
    = NoTreasuryAction
    | MergeTreasuryUtxos MergeTreasuryUtxosState


type alias MergeTreasuryUtxosState =
    { scopeName : String
    , scope : Scope
    , rootUtxo : OutputReference
    , status : MergeStatus
    }


type MergeStatus
    = MergeIdle
    | MergeBuilding (Result TxFinalizationError TxFinalized)
    | MergeAwaitingSignature Transaction
    | MergeSubmitting Transaction


init : Flags -> ( Model, Cmd Msg )
init { db, pragmaScriptHash, registriesSeedUtxo, treasuryConfigExpiration, blueprints } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        registryBlueprint =
            List.Extra.find (\{ name } -> name == "traps.treasury_registry.spend") decodedBlueprints

        amaruTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "permissions.permissions.withdraw") decodedBlueprints

        ( scripts, blueprintError ) =
            case ( sundaeTreasuryBlueprint, registryBlueprint, amaruTreasuryBlueprint ) of
                ( Just treasury, Just registry, Just amaru ) ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 treasury.scriptBytes
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 registry.scriptBytes
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 amaru.scriptBytes
                      }
                    , Nothing
                    )

                _ ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Just "Failed the retrieve some of the Plutus blueprints. Did you forget to build the aiken code?"
                    )

        model =
            initialModel db pragmaScriptHash ( registriesSeedUtxo.transactionId, registriesSeedUtxo.outputIndex ) treasuryConfigExpiration scripts

        -- Load Pragma UTxO
        ( updatedTaskPool, loadPragmaUtxoCmd ) =
            Api.retrieveAssetUtxo model.networkId model.pragmaScriptHash (Bytes.fromText "amaru scopes")
                |> ConcurrentTask.mapError Debug.toString
                |> ConcurrentTask.andThen (Api.retrieveOutput model.networkId)
                |> ConcurrentTask.map LoadedPragmaUtxo
                |> ConcurrentTask.attempt
                    { pool = model.taskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }

        loadingTreasuryResult =
            initLoadingTreasury
                model.scripts.scopePermissions
                model.pragmaScriptHash
                model.scripts.registryTrap
                model.registriesSeedUtxo
                model.treasuryConfigExpiration

        loadRegistryUtxos : Scopes LoadingScope -> ConcurrentTask String (Scopes ( OutputReference, Output ))
        loadRegistryUtxos loadingScopes =
            let
                registryAssets =
                    Scopes
                        ( loadingScopes.ledger.registryNftPolicyId, Bytes.fromText "ledger" )
                        ( loadingScopes.consensus.registryNftPolicyId, Bytes.fromText "consensus" )
                        ( loadingScopes.mercenaries.registryNftPolicyId, Bytes.fromText "mercenaries" )
                        ( loadingScopes.marketing.registryNftPolicyId, Bytes.fromText "marketing" )

                -- Query assets utxos
                assetsUtxosTask : ConcurrentTask String (MultiAsset (Utxo.RefDict Output))
                assetsUtxosTask =
                    Api.retrieveAssetsUtxos { db = db }
                        model.networkId
                        [ registryAssets.ledger
                        , registryAssets.consensus
                        , registryAssets.mercenaries
                        , registryAssets.marketing
                        ]

                extractLoadedUtxos : MultiAsset (Utxo.RefDict Output) -> ConcurrentTask String (Scopes ( OutputReference, Output ))
                extractLoadedUtxos assetsUtxos =
                    Result.map4 Scopes
                        (extractRegistryUtxo registryAssets.ledger assetsUtxos)
                        (extractRegistryUtxo registryAssets.consensus assetsUtxos)
                        (extractRegistryUtxo registryAssets.mercenaries assetsUtxos)
                        (extractRegistryUtxo registryAssets.marketing assetsUtxos)
                        |> ConcurrentTask.fromResult

                extractRegistryUtxo ( policyId, assetName ) multiAsset =
                    case MultiAsset.get policyId assetName multiAsset of
                        Nothing ->
                            Err <| "Missing asset: " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName

                        Just refDict ->
                            case Dict.Any.toList refDict of
                                [] ->
                                    Err <| "Missing asset: " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName

                                [ utxo ] ->
                                    Ok utxo

                                _ ->
                                    Err <| "Asset in more than 1 outputs! : " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName
            in
            ConcurrentTask.andThen extractLoadedUtxos assetsUtxosTask

        attemptLoadRegistryUtxosTask loadingScopes =
            loadRegistryUtxos loadingScopes
                |> ConcurrentTask.map LoadedRegistryUtxos
                |> ConcurrentTask.attempt
                    { pool = updatedTaskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }
    in
    case ( blueprintError, loadingTreasuryResult ) of
        ( Nothing, Ok loadingTreasury ) ->
            let
                ( updatedAgainTaskPool, loadRegistryUtxosCmd ) =
                    attemptLoadRegistryUtxosTask loadingTreasury.scopes
            in
            ( { model
                | taskPool = updatedAgainTaskPool
                , treasuryManagement = TreasuryLoading loadingTreasury
              }
            , Cmd.batch
                [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
                , Api.loadProtocolParams model.networkId GotNetworkParams
                , loadPragmaUtxoCmd
                , loadRegistryUtxosCmd
                ]
            )

        ( Just error, _ ) ->
            ( { model | error = Just error }, Cmd.none )

        ( _, Err error ) ->
            ( { model | error = Just error }, Cmd.none )


blueprintDecoder : JD.Decoder (List ScriptBlueprint)
blueprintDecoder =
    JD.at [ "validators" ]
        (JD.list
            (JD.map4 ScriptBlueprint
                (JD.field "title" JD.string)
                (JD.field "compiledCode" JD.string |> JD.map Bytes.fromHexUnchecked)
                (JD.field "hash" JD.string |> JD.map Bytes.fromHexUnchecked)
                (JD.maybe (JD.field "parameters" JD.value) |> JD.map (\p -> Maybe.map (always True) p |> Maybe.withDefault False))
            )
        )


type alias ScriptBlueprint =
    { name : String
    , scriptBytes : Bytes ScriptCbor
    , hash : Bytes CredentialHash
    , hasParams : Bool
    }



-- UPDATE ############################################################


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( Debug.todo "", Debug.todo "" )

        WalletMsg value ->
            handleWalletMsg value model

        DisconnectWallet ->
            ( { model | connectedWallet = Nothing }, Cmd.none )

        ConnectButtonClicked { id } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Just 3 })) )

        GotNetworkParams (Err httpError) ->
            ( { model | error = Just <| Debug.toString httpError }, Cmd.none )

        GotNetworkParams (Ok params) ->
            ( { model | protocolParams = params }, Cmd.none )

        TreasuryMergingMsg submsg ->
            handleTreasuryMergingMsg submsg model

        OnTaskProgress ( taskPool, cmd ) ->
            ( { model | taskPool = taskPool }, cmd )

        OnTaskComplete taskCompleted ->
            handleTask taskCompleted model



-- Wallet


walletResponseDecoder : JD.Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.singleton 30 Cip30.apiDecoder


handleWalletMsg : JD.Value -> Model -> ( Model, Cmd Msg )
handleWalletMsg value model =
    case JD.decodeValue walletResponseDecoder value of
        -- We just discovered available wallets
        Ok (Cip30.AvailableWallets wallets) ->
            ( { model | discoveredWallets = wallets }, Cmd.none )

        -- We just connected to the wallet, let’s ask for the available utxos
        Ok (Cip30.EnabledWallet wallet) ->
            ( { model | connectedWallet = Just wallet }
            , toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
            )

        -- We just received the utxos, let’s add them to the local state
        Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)) ->
            ( { model | localStateUtxos = List.foldl (\( ref, output ) -> Dict.Any.insert ref output) model.localStateUtxos utxos }
            , Cmd.none
            )

        -- We just received a CIP-30 api error from the wallet
        Ok (Cip30.ApiError { code, info }) ->
            ( { model | error = Just <| "Wallet Error (code " ++ String.fromInt code ++ "):\n" ++ info }
            , Cmd.none
            )

        -- TODO: handle the error case
        _ ->
            ( model, Cmd.none )



-- Merge UTxOs


type TreasuryMergingMsg
    = StartMergeUtxos String Scope OutputReference
    | BuildMergeTransaction (List (Bytes CredentialHash))
    | SignMergeTransaction Transaction
    | SubmitMergeTransaction Transaction
    | CancelMergeAction


handleTreasuryMergingMsg : TreasuryMergingMsg -> Model -> ( Model, Cmd Msg )
handleTreasuryMergingMsg msg model =
    case msg of
        StartMergeUtxos scopeName scope rootUtxo ->
            handleStartMergeUtxos scopeName scope rootUtxo model

        BuildMergeTransaction requiredSigners ->
            handleBuildMergeTransaction model requiredSigners

        SignMergeTransaction transaction ->
            handleSignMergeTransaction transaction model

        SubmitMergeTransaction transaction ->
            handleSubmitMergeTransaction transaction model

        CancelMergeAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none )


handleStartMergeUtxos : String -> Scope -> OutputReference -> Model -> ( Model, Cmd Msg )
handleStartMergeUtxos scopeName scope rootUtxo model =
    if Dict.Any.size scope.treasuryUtxos > 1 then
        let
            mergeState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = MergeIdle
                }
        in
        ( { model | treasuryAction = MergeTreasuryUtxos mergeState }
        , Cmd.none
        )

    else
        ( { model | error = Just "Scope has insufficient UTXOs to merge" }
        , Cmd.none
        )


handleBuildMergeTransaction : Model -> List (Bytes CredentialHash) -> ( Model, Cmd Msg )
handleBuildMergeTransaction model requiredSigners =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                mergeTxIntents =
                    mergeUtxos model.networkId mergeState.scope requiredSigners

                -- TODO: update TxOtherInfo
                txOtherInfo =
                    [ TxIntent.TxReferenceInput mergeState.rootUtxo ]

                feeSource =
                    Cip30.walletChangeAddress wallet

                txResult =
                    TxIntent.finalizeAdvanced
                        { govState = TxIntent.emptyGovernanceState
                        , localStateUtxos = model.localStateUtxos
                        , coinSelectionAlgo = CoinSelection.largestFirst
                        , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource mergeTxIntents
                        , costModels = Uplc.conwayDefaultCostModels
                        }
                        (TxIntent.AutoFee { paymentSource = feeSource })
                        txOtherInfo
                        mergeTxIntents

                updatedMergeState =
                    { mergeState | status = MergeBuilding txResult }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }
            in
            ( updatedModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleSignMergeTransaction : Transaction -> Model -> ( Model, Cmd Msg )
handleSignMergeTransaction tx model =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                updatedMergeState =
                    { mergeState | status = MergeAwaitingSignature tx }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }

                signCmd =
                    toWallet <| Cip30.encodeRequest <| Cip30.signTx wallet { partialSign = True } tx
            in
            ( updatedModel, signCmd )

        _ ->
            ( model, Cmd.none )


handleSubmitMergeTransaction : Transaction -> Model -> ( Model, Cmd Msg )
handleSubmitMergeTransaction tx model =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                updatedMergeState =
                    { mergeState | status = MergeSubmitting tx }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }

                submitCmd =
                    toWallet <| Cip30.encodeRequest <| Cip30.submitTx wallet tx
            in
            ( updatedModel, submitCmd )

        _ ->
            ( model, Cmd.none )


{-| Merge all UTxOs from a given scope.

REMARK: you also need to add a `TxIntent.TxReferenceInput rootUtxoRef`

-}
mergeUtxos : NetworkId -> Scope -> List (Bytes CredentialHash) -> List TxIntent
mergeUtxos networkId scope requiredSigners =
    let
        utxos =
            Dict.Any.toList scope.treasuryUtxos

        treasuryScriptBytes =
            Script.cborWrappedBytes <| Tuple.second scope.sundaeTreasuryScript

        requiredWithdrawals =
            -- Withdrawal with the scope owner script
            [ { stakeCredential =
                    { networkId = networkId
                    , stakeCredential = ScriptHash ownerScriptHash
                    }
              , amount = N.zero
              , scriptWitness =
                    Just <|
                        Witness.Plutus
                            { script =
                                ( Script.plutusVersion permissionsScript
                                , Witness.ByValue <| Script.cborWrappedBytes permissionsScript
                                )
                            , redeemerData = \_ -> Data.Constr N.zero []
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ( ownerScriptHash, permissionsScript ) =
            scope.permissionsScript

        receivers value =
            [ TxIntent.SendTo scopeTreasuryAddress value ]

        scopeTreasuryAddress =
            case utxos of
                ( _, { address } ) :: _ ->
                    address

                _ ->
                    Debug.todo "impossible to have utxos = []"
    in
    Treasury.reorganize treasuryScriptBytes requiredSigners requiredWithdrawals utxos receivers



-- Disburse


{-| Disburse funds from one UTxO in the given scope.

REMARK: you also need to add a `TxIntent.TxReferenceInput rootUtxoRef`

-}
disburse : NetworkId -> Scope -> List (Bytes CredentialHash) -> OutputReference -> (Value -> List TxIntent) -> Value -> Result String (List TxIntent)
disburse networkId scope requiredSigners utxoRef receivers value =
    case Dict.Any.get utxoRef scope.treasuryUtxos of
        Nothing ->
            Err <| "The selected UTxO isn’t in the known list of UTxOs for this scope: " ++ Debug.toString utxoRef

        Just spentOutput ->
            let
                spendConfig : SpendConfig
                spendConfig =
                    { treasuryScriptBytes = Script.cborWrappedBytes <| Tuple.second scope.sundaeTreasuryScript
                    , requiredSigners = requiredSigners
                    , requiredWithdrawals = requiredWithdrawals
                    , spentInputRef = utxoRef
                    , spentOutput = spentOutput
                    }

                requiredWithdrawals =
                    -- Withdrawal with the scope owner script
                    [ { stakeCredential =
                            { networkId = networkId
                            , stakeCredential = ScriptHash ownerScriptHash
                            }
                      , amount = N.zero
                      , scriptWitness =
                            Just <|
                                Witness.Plutus
                                    { script =
                                        ( Script.plutusVersion permissionsScript
                                        , Witness.ByValue <| Script.cborWrappedBytes permissionsScript
                                        )
                                    , redeemerData = \_ -> Debug.todo "Standard/Swap Owner Redeemer"
                                    , requiredSigners = requiredSigners
                                    }
                      }
                    ]

                ( ownerScriptHash, permissionsScript ) =
                    scope.permissionsScript

                overflowValue =
                    Value.subtract value spentOutput.amount
            in
            if overflowValue == Value.zero then
                Ok <| Treasury.disburse spendConfig receivers value

            else
                Err <| "Trying to disburse more than is available in this UTxO. Overflow value is: " ++ Debug.toString overflowValue



-- Tasks


handleTask : ConcurrentTask.Response String TaskCompleted -> Model -> ( Model, Cmd Msg )
handleTask response model =
    case ( response, model.page ) of
        ( ConcurrentTask.Error error, _ ) ->
            ( { model | error = Just error }, Cmd.none )

        ( ConcurrentTask.UnexpectedError error, _ ) ->
            ( { model | error = Just <| Debug.toString error }, Cmd.none )

        ( ConcurrentTask.Success taskCompleted, _ ) ->
            handleCompletedTask model taskCompleted


handleCompletedTask : Model -> TaskCompleted -> ( Model, Cmd Msg )
handleCompletedTask model taskCompleted =
    case taskCompleted of
        LoadedPragmaUtxo ( ref, output ) ->
            case model.treasuryManagement of
                TreasuryLoading loadingTreasury ->
                    case setPragmaUtxo model.scripts.sundaeTreasury ref output loadingTreasury of
                        Ok treasuryManagement ->
                            -- Also create a task to retrieve the scopes treasuries UTxOs
                            -- now that we have the applied sundae treasury scripts for each scope
                            let
                                allTreasuriesUtxosTask : ConcurrentTask String (BytesMap CredentialHash (Utxo.RefDict Output))
                                allTreasuriesUtxosTask =
                                    -- The list order is the same as the order of the scopes
                                    Api.retrieveUtxosWithPaymentCreds { db = model.db } model.networkId allTreasuriesScriptHashes

                                allTreasuriesScriptHashes =
                                    List.filterMap extractAppliedTreasuryScriptHash loadingScopes

                                loadingScopes =
                                    case treasuryManagement of
                                        TreasuryLoading { scopes } ->
                                            -- The list order is the same as the order of the scopes
                                            [ scopes.ledger, scopes.consensus, scopes.mercenaries, scopes.marketing ]

                                        _ ->
                                            []

                                extractAppliedTreasuryScriptHash { sundaeTreasuryScriptApplied } =
                                    RemoteData.map (\( hash, _ ) -> hash) sundaeTreasuryScriptApplied
                                        |> RemoteData.toMaybe

                                ( updatedTaskPool, loadScopeUtxosCmd ) =
                                    allTreasuriesUtxosTask
                                        |> ConcurrentTask.map bytesMapToList
                                        |> ConcurrentTask.andThen listToScopes
                                        |> ConcurrentTask.map LoadedTreasuriesUtxos
                                        |> ConcurrentTask.attempt
                                            { pool = model.taskPool
                                            , send = sendTask
                                            , onComplete = OnTaskComplete
                                            }

                                bytesMapToList bytesMap =
                                    List.filterMap (\hash -> Bytes.Map.get hash bytesMap) allTreasuriesScriptHashes

                                listToScopes utxosInList =
                                    case utxosInList of
                                        [ ledgerUtxos, consensusUtxos, mercenariesUtxos, marketingUtxos ] ->
                                            ConcurrentTask.succeed (Scopes ledgerUtxos consensusUtxos mercenariesUtxos marketingUtxos)

                                        _ ->
                                            ConcurrentTask.fail "Something unexpected happened while trying to retrieve scopes treasuries UTxOs"
                            in
                            ( { model
                                | treasuryManagement = treasuryManagement
                                , taskPool = updatedTaskPool
                              }
                            , loadScopeUtxosCmd
                            )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadedRegistryUtxos registryUtxos ->
            case model.treasuryManagement of
                TreasuryLoading ({ scopes } as loadingTreasury) ->
                    -- If all UTxOs have been loaded (registry and scope)
                    -- then we can convert into a LoadedTreasury
                    ( { model
                        | treasuryManagement =
                            TreasuryLoading { loadingTreasury | scopes = setRegistryUtxos registryUtxos scopes }
                      }
                        |> upgradeIfTreasuryLoadingFinished
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedTreasuriesUtxos treasuriesUtxos ->
            case model.treasuryManagement of
                TreasuryLoading ({ scopes } as loadingTreasury) ->
                    -- If all UTxOs have been loaded (registry and scope)
                    -- then we can convert into a LoadedTreasury
                    ( { model
                        | treasuryManagement =
                            TreasuryLoading { loadingTreasury | scopes = setTreasuryUtxos treasuriesUtxos scopes }
                      }
                        |> upgradeIfTreasuryLoadingFinished
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


setRegistryUtxos : Scopes ( OutputReference, Output ) -> Scopes LoadingScope -> Scopes LoadingScope
setRegistryUtxos registryUtxos { ledger, consensus, mercenaries, marketing } =
    { ledger = { ledger | registryUtxo = RemoteData.Success registryUtxos.ledger }
    , consensus = { consensus | registryUtxo = RemoteData.Success registryUtxos.consensus }
    , mercenaries = { mercenaries | registryUtxo = RemoteData.Success registryUtxos.mercenaries }
    , marketing = { marketing | registryUtxo = RemoteData.Success registryUtxos.marketing }
    }


setTreasuryUtxos : Scopes (Utxo.RefDict Output) -> Scopes LoadingScope -> Scopes LoadingScope
setTreasuryUtxos treasuryUtxos { ledger, consensus, mercenaries, marketing } =
    { ledger = { ledger | treasuryUtxos = RemoteData.Success treasuryUtxos.ledger }
    , consensus = { consensus | treasuryUtxos = RemoteData.Success treasuryUtxos.consensus }
    , mercenaries = { mercenaries | treasuryUtxos = RemoteData.Success treasuryUtxos.mercenaries }
    , marketing = { marketing | treasuryUtxos = RemoteData.Success treasuryUtxos.marketing }
    }


{-| Extract the scopes owner config from the root pragma utxo.
Also apply the Sundae treasuy contracts with the now known paramaters.
-}
setPragmaUtxo : PlutusScript -> OutputReference -> Output -> LoadingTreasury -> Result String TreasuryManagement
setPragmaUtxo unappliedSundaeTreasuryScript ref output loading =
    let
        scopes =
            case output.datumOption of
                Just (DatumValue { rawBytes }) ->
                    case Data.fromBytes rawBytes of
                        Nothing ->
                            Err "The PRAGMA UTxO datum does not contain valid Data."

                        Just data ->
                            updateLoadingScopesFromData unappliedSundaeTreasuryScript data loading.scopes

                _ ->
                    Err "The PRAGMA UTxO does not contain a valid datum"

        treasuryLoading okScopes =
            TreasuryLoading
                { loading
                    | rootUtxo = RemoteData.Success ( ref, output )
                    , scopes = okScopes
                }
    in
    Result.map treasuryLoading scopes


updateLoadingScopesFromData : PlutusScript -> Data -> Scopes LoadingScope -> Result String (Scopes LoadingScope)
updateLoadingScopesFromData unappliedSundaeTreasuryScript data { ledger, consensus, mercenaries, marketing } =
    case data of
        Data.Constr _ [ owner1, owner2, owner3, owner4 ] ->
            case ( ( MultisigScript.fromData owner1, MultisigScript.fromData owner2 ), ( MultisigScript.fromData owner3, MultisigScript.fromData owner4 ) ) of
                ( ( Just ms1, Just ms2 ), ( Just ms3, Just ms4 ) ) ->
                    let
                        updateOwner scope owner =
                            { scope
                                | owner = Just owner
                                , sundaeTreasuryScriptApplied = applySundaeTreasuryScript unappliedSundaeTreasuryScript scope
                            }
                    in
                    Ok
                        { ledger = updateOwner ledger ms1
                        , consensus = updateOwner consensus ms2
                        , mercenaries = updateOwner mercenaries ms3
                        , marketing = updateOwner marketing ms4
                        }

                ( ( Nothing, _ ), _ ) ->
                    Err "The first scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( ( _, Nothing ), _ ) ->
                    Err "The second scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( Nothing, _ ) ) ->
                    Err "The third scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( _, Nothing ) ) ->
                    Err "The fourth scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

        _ ->
            Err "The PRAGMA UTxO does not contain exactly 4 owners"


applySundaeTreasuryScript : PlutusScript -> LoadingScope -> RemoteData String ( Bytes CredentialHash, PlutusScript )
applySundaeTreasuryScript unappliedScript scope =
    let
        multisig =
            MultisigScript.Script (Tuple.first scope.permissionsScriptApplied)

        treasuryConfig : Types.TreasuryConfiguration
        treasuryConfig =
            { registryToken = scope.registryNftPolicyId
            , permissions =
                { reorganize = multisig
                , sweep = multisig
                , fund = MultisigScript.AnyOf []
                , disburse = multisig
                , unregister = multisig
                }
            , expiration = scope.expiration
            , payoutUpperbound = N.zero
            }
    in
    Uplc.applyParamsToScript [ Types.treasuryConfigToData treasuryConfig ] unappliedScript
        |> Result.Extra.unpack RemoteData.Failure
            (\applied -> RemoteData.Success ( Script.hash <| Script.Plutus applied, applied ))


upgradeIfTreasuryLoadingFinished : Model -> Model
upgradeIfTreasuryLoadingFinished model =
    case model.treasuryManagement of
        TreasuryLoading { rootUtxo, scopes } ->
            case ( rootUtxo, upgradeScopesIfLoadingFinished scopes ) of
                ( RemoteData.Success ( ref, output ), Just loadedScopes ) ->
                    -- Upgrade the treasury management
                    -- AND the local state utxos
                    { model
                        | treasuryManagement = TreasuryFullyLoaded { rootUtxo = ( ref, output ), scopes = loadedScopes }
                        , localStateUtxos = addLoadedUtxos ( ref, output ) loadedScopes model.localStateUtxos
                    }

                _ ->
                    model

        _ ->
            model


upgradeScopesIfLoadingFinished : Scopes LoadingScope -> Maybe (Scopes Scope)
upgradeScopesIfLoadingFinished { ledger, consensus, mercenaries, marketing } =
    Maybe.map4 Scopes
        (upgradeScope ledger)
        (upgradeScope consensus)
        (upgradeScope mercenaries)
        (upgradeScope marketing)


upgradeScope : LoadingScope -> Maybe Scope
upgradeScope scope =
    case ( scope.owner, ( scope.sundaeTreasuryScriptApplied, scope.registryUtxo, scope.treasuryUtxos ) ) of
        ( Just owner, ( RemoteData.Success treasuryScriptApplied, RemoteData.Success registryUtxo, RemoteData.Success treasuryUtxos ) ) ->
            Just
                { owner = owner
                , permissionsScript = scope.permissionsScriptApplied
                , sundaeTreasuryScript = treasuryScriptApplied
                , registryUtxo = registryUtxo
                , treasuryUtxos = treasuryUtxos
                }

        _ ->
            Nothing


addLoadedUtxos : ( OutputReference, Output ) -> Scopes Scope -> Utxo.RefDict Output -> Utxo.RefDict Output
addLoadedUtxos ( rootRef, rootOutput ) { ledger, consensus, mercenaries, marketing } localState =
    let
        addScopeUtxos scope accum =
            Dict.Any.union scope.treasuryUtxos accum
                |> Dict.Any.insert (Tuple.first scope.registryUtxo) (Tuple.second scope.registryUtxo)
    in
    addScopeUtxos ledger localState
        |> addScopeUtxos consensus
        |> addScopeUtxos mercenaries
        |> addScopeUtxos marketing
        |> Dict.Any.insert rootRef rootOutput



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    div []
        [ viewError model.error
        , viewWalletSection model
        , viewLocalStateUtxosSection model.localStateUtxos
        , case model.treasuryAction of
            NoTreasuryAction ->
                viewTreasurySection model.treasuryManagement

            MergeTreasuryUtxos mergeState ->
                viewMergeUtxosAction model.connectedWallet mergeState
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            Html.pre [] [ text <| "ERROR:\n" ++ error ]



-- Wallet


viewWalletSection : Model -> Html Msg
viewWalletSection { discoveredWallets, connectedWallet } =
    case connectedWallet of
        Nothing ->
            div []
                [ div [] [ text "CIP-30 wallets detected:" ]
                , viewAvailableWallets discoveredWallets
                ]

        Just wallet ->
            viewConnectedWallet wallet


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        connectButton { id } =
            Html.button [ onClick (ConnectButtonClicked { id = id }) ] [ text "connect" ]

        walletRow w =
            div [] [ walletIcon w, text (walletDescription w), connectButton w ]
    in
    div [] (List.map walletRow wallets)


viewConnectedWallet : Cip30.Wallet -> Html Msg
viewConnectedWallet wallet =
    div []
        [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
        , div [] [ text <| "Address: " ++ (Address.toBech32 <| Cip30.walletChangeAddress wallet) ]
        , Html.button [ onClick DisconnectWallet ] [ text "Disconnect" ]
        ]



-- Local state UTxOs


viewLocalStateUtxosSection : Utxo.RefDict Output -> Html msg
viewLocalStateUtxosSection utxos =
    Html.p [] [ text <| "Local state UTxOs size: " ++ String.fromInt (Dict.Any.size utxos) ]



-- Treasury Section


viewTreasurySection : TreasuryManagement -> Html Msg
viewTreasurySection treasuryManagement =
    case treasuryManagement of
        TreasuryUnspecified ->
            div [] [ text "Treasury unspecified yet" ]

        TreasuryLoading { rootUtxo, scopes } ->
            div []
                [ Html.p [] [ text "Loading treasury ... ", spinner ]
                , viewLoadingRootUtxo rootUtxo
                , viewLoadingScope "ledger" scopes.ledger
                , viewLoadingScope "consensus" scopes.consensus
                , viewLoadingScope "mercenaries" scopes.mercenaries
                , viewLoadingScope "marketing" scopes.marketing
                ]

        TreasuryFullyLoaded { rootUtxo, scopes } ->
            let
                rootUtxoRef =
                    Tuple.first rootUtxo
            in
            div []
                [ Html.p [] [ text "Treasury fully loaded" ]
                , viewRootUtxo rootUtxo
                , viewScope rootUtxoRef "ledger" scopes.ledger
                , viewScope rootUtxoRef "consensus" scopes.consensus
                , viewScope rootUtxoRef "mercenaries" scopes.mercenaries
                , viewScope rootUtxoRef "marketing" scopes.marketing
                ]


viewLoadingRootUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewLoadingRootUtxo rootUtxo =
    case rootUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text "PRAGMA root UTxO not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text "PRAGMA root UTxO loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "PRAGMA root UTxO failed to load: " ++ error ]

        RemoteData.Success utxo ->
            viewRootUtxo utxo


viewRootUtxo : ( OutputReference, Output ) -> Html msg
viewRootUtxo ( ref, _ ) =
    Html.p [] [ text <| "PRAGMA root UTxO: " ++ Utxo.refAsString ref ]


viewLoadingScope : String -> LoadingScope -> Html msg
viewLoadingScope scopeName { owner, permissionsScriptApplied, sundaeTreasuryScriptApplied, registryNftPolicyId, registryUtxo, treasuryUtxos } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewMaybeOwner owner
        , viewPermissionsScript permissionsScriptApplied
        , viewLoadingTreasuryScript sundaeTreasuryScriptApplied
        , viewRegistryNftPolicyId registryNftPolicyId
        , viewLoadingRegistryUtxo registryUtxo
        , viewLoadingTreasuryUtxos treasuryUtxos
        ]


viewScope : OutputReference -> String -> Scope -> Html Msg
viewScope rootUtxo scopeName ({ owner, permissionsScript, sundaeTreasuryScript, registryUtxo, treasuryUtxos } as scope) =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewTreasuryScript sundaeTreasuryScript
        , viewRegistryUtxo registryUtxo
        , viewTreasuryUtxos scopeName scope rootUtxo treasuryUtxos
        ]


viewMaybeOwner : Maybe MultisigScript -> Html msg
viewMaybeOwner maybeOwner =
    case maybeOwner of
        Nothing ->
            Html.p [] [ text <| "Owner: loading ... ", spinner ]

        Just owner ->
            viewOwner owner


viewOwner : MultisigScript -> Html msg
viewOwner owner =
    Html.p [] [ text <| "Owner: " ++ Debug.toString owner ]


viewPermissionsScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewPermissionsScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied permissions script hash: " ++ Bytes.toHex hash ]


viewLoadingTreasuryScript : RemoteData String ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewLoadingTreasuryScript remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Sundae treasury script not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Sundae treasury script loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Sundae treasury script failed to load: " ++ error ]

        RemoteData.Success script ->
            viewTreasuryScript script


viewTreasuryScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewTreasuryScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied Sundae treasury script hash: " ++ Bytes.toHex hash ]


viewRegistryNftPolicyId : Bytes PolicyId -> Html msg
viewRegistryNftPolicyId policyId =
    Html.p [] [ text <| "Registry trap policy ID: " ++ Bytes.toHex policyId ]


viewLoadingRegistryUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewLoadingRegistryUtxo registryUtxo =
    case registryUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Registry UTxO not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Registry UTxO loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Registry UTxO failed to load: " ++ error ]

        RemoteData.Success utxo ->
            viewRegistryUtxo utxo


viewRegistryUtxo : ( OutputReference, Output ) -> Html msg
viewRegistryUtxo ( ref, _ ) =
    Html.p [] [ text <| "Registry UTxO: " ++ Utxo.refAsString ref ]


viewLoadingTreasuryUtxos : RemoteData String (Utxo.RefDict Output) -> Html msg
viewLoadingTreasuryUtxos loadingUtxos =
    case loadingUtxos of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Treasury UTxOs not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Treasury UTxOs loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Treasury UTxOs failed to load: " ++ error ]

        RemoteData.Success utxos ->
            Html.p [] [ text <| "Treasury UTxOs loaded. UTxO count = " ++ String.fromInt (Dict.Any.size utxos) ]


viewTreasuryUtxos : String -> Scope -> OutputReference -> Utxo.RefDict Output -> Html Msg
viewTreasuryUtxos scopeName scope rootUtxo utxos =
    let
        mergeButton =
            if Dict.Any.size utxos > 1 then
                Html.button [ onClick (StartMergeUtxos scopeName scope rootUtxo) ] [ text "Merge UTxOs" ]

            else
                text ""
    in
    div []
        [ Html.p [] [ text <| "Treasury UTxOs count: " ++ String.fromInt (Dict.Any.size utxos) ]
        , Html.p [] [ text <| "TODO: add buttons for possible actions with those UTxOs" ]
        , Html.map TreasuryMergingMsg mergeButton
        , Html.ul [] <|
            List.map viewDetailedUtxo (Dict.Any.toList utxos)
        ]


viewDetailedUtxo : ( OutputReference, Output ) -> Html msg
viewDetailedUtxo ( ref, output ) =
    Html.li []
        [ div [] [ text <| "UTxO: " ++ Utxo.refAsString ref ]
        , div [] [ text <| "Address: " ++ Address.toBech32 output.address ]
        , div [] [ text <| "Value: (₳ amounts are in Lovelaces)" ]
        , Html.pre [] [ text <| String.join "\n" <| Value.toMultilineString output.amount ]
        ]


spinner : Html msg
spinner =
    Html.span [ HA.class "loader" ] []



-- Merge UTxOs


viewMergeUtxosAction : Maybe Cip30.Wallet -> MergeTreasuryUtxosState -> Html Msg
viewMergeUtxosAction maybeWallet mergeState =
    Html.map TreasuryMergingMsg <|
        div []
            [ Html.h3 [] [ text ("Merge UTXOs - " ++ mergeState.scopeName ++ " Scope") ]
            , div [] [ text ("UTXOs to merge: " ++ String.fromInt (Dict.Any.size mergeState.scope.treasuryUtxos)) ]
            , Html.ul [] <|
                List.map viewDetailedUtxo (Dict.Any.toList mergeState.scope.treasuryUtxos)
            , case mergeState.status of
                MergeIdle ->
                    let
                        -- FIXME: temporary, something more robust should be done,
                        -- and not inside the view, but this works as a MVP.
                        requiredSigners =
                            MultisigScript.extractRequiredSigners mergeState.scope.owner
                    in
                    case maybeWallet of
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

                MergeBuilding txFinalizedResult ->
                    case txFinalizedResult of
                        Err error ->
                            div []
                                [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                                , Html.pre [] [ text <| TxIntent.errorToString error ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                        Ok { tx, expectedSignatures } ->
                            div []
                                [ Html.p [] [ text "Merge Tx to be signed:" ]
                                , Html.pre [] [ text <| prettyTx tx ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                MergeAwaitingSignature transaction ->
                    div []
                        [ Html.p [] [ text "Transaction built successfully. Please sign the transaction." ]
                        , Html.button [ onClick (SignMergeTransaction transaction) ] [ text "Sign Transaction" ]
                        , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                        ]

                MergeSubmitting transaction ->
                    div [] [ text " Submitting transaction...", spinner ]
            ]
