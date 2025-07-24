port module Main exposing (Flags, Model, Msg, Page, TaskCompleted, main)

import Api exposing (ProtocolParams)
import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map
import Cardano.Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent
import Cardano.Utxo as Utxo exposing (Output, TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import Dict
import Dict.Any
import Header
import Html exposing (Html, div, text)
import Http
import Json.Decode as JD
import List.Extra
import Page.Home
import Page.Loading as Loading exposing (Loaded, Loading)
import Page.Setup as Setup
import Page.SignTx as SignTx
import Page.Treasury as Treasury
import Platform.Cmd as Cmd
import Result.Extra
import Route exposing (Route)
import Storage
import Task
import Time exposing (Posix)
import Treasury.LoadingParams as LoadingParams
import Treasury.Scope exposing (Scripts)


type alias Flags =
    { url : String
    , db : JD.Value
    , blueprints : List JD.Value
    , posixTimeMs : Int
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
                    , onUrlChange (UrlChanged << Route.fromLocationHref)
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
    | UrlChanged Route
    | WalletMsg JD.Value
    | DisconnectWallet
    | ConnectButtonClicked { id : String }
    | GotNetworkParams (Result Http.Error ProtocolParams)
      -- Signature page
    | SignTxMsg SignTx.Msg
      -- Treasury management
    | StartTreasurySetup
    | StartTreasurySetupWithCurrentTime Posix
    | SetupMsg Setup.Msg
    | StartTreasuryLoading
    | LoadingParamsMsg LoadingParams.Msg
    | TreasuryMsg Treasury.Msg
    | RefreshUtxos Loaded
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = ReadLoadingParams (Maybe LoadingParams.Form)
    | TreasuryLoadingTask Loading.TaskCompleted



-- MODEL


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg String TaskCompleted
    , db : JD.Value
    , page : Page
    , appUrl : AppUrl
    , networkId : NetworkId
    , protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , scripts : Scripts
    , treasuryLoadingParamsForm : LoadingParams.Form
    , error : Maybe String
    }


initialModel : JD.Value -> Scripts -> Int -> Model
initialModel db scripts posixTimeMs =
    let
        treasuryLoadingParamsForm =
            { pragmaScriptHash = ""
            , registriesSeedUtxo = { transactionId = "", outputIndex = 0 }
            , treasuryConfigExpiration = posixTimeMs
            , error = Nothing
            }
    in
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = HomePage
    , appUrl = Route.toAppUrl Route.Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , scripts = scripts
    , treasuryLoadingParamsForm = treasuryLoadingParamsForm
    , error = Nothing
    }


type Page
    = HomePage
    | SetupPage Setup.Model
    | LoadingPage Loading
    | TreasuryPage Treasury.Model
    | SignTxPage { previousPage : Page } SignTx.Model


init : Flags -> ( Model, Cmd Msg )
init { url, db, blueprints, posixTimeMs } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        registryBlueprint =
            List.Extra.find (\{ name } -> name == "traps.treasury_registry.spend") decodedBlueprints

        scopesBlueprint =
            List.Extra.find (\{ name } -> name == "traps.scopes.mint") decodedBlueprints

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        permissionsBlueprint =
            List.Extra.find (\{ name } -> name == "permissions.permissions.withdraw") decodedBlueprints

        ( scripts, blueprintError ) =
            case ( ( registryBlueprint, scopesBlueprint ), ( sundaeTreasuryBlueprint, permissionsBlueprint ) ) of
                ( ( Just registry, Just scopes ), ( Just treasury, Just permissions ) ) ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 treasury.scriptBytes
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 registry.scriptBytes
                      , scopesTrap = Script.plutusScriptFromBytes PlutusV3 scopes.scriptBytes
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 permissions.scriptBytes
                      }
                    , Nothing
                    )

                _ ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopesTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Just "Failed the retrieve some of the Plutus blueprints. Did you forget to build the aiken code?"
                    )

        ( model, routingCmd ) =
            handleUrlChange (Route.fromLocationHref url)
                (initialModel db scripts posixTimeMs)

        ( updatedTaskPool, readTreasuryParamsCmd ) =
            Storage.read { db = db, storeName = "stuff" } LoadingParams.formDecoder { key = "treasuryLoadingParams" }
                |> ConcurrentTask.map Just
                |> ConcurrentTask.onError (\_ -> ConcurrentTask.succeed Nothing)
                |> ConcurrentTask.map ReadLoadingParams
                |> ConcurrentTask.attempt
                    { pool = model.taskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }
    in
    case blueprintError of
        Nothing ->
            ( { model | taskPool = updatedTaskPool }
            , Cmd.batch
                [ routingCmd
                , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
                , Api.loadProtocolParams model.networkId GotNetworkParams
                , readTreasuryParamsCmd
                ]
            )

        Just error ->
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


type alias UpdateContext msg =
    { toMsg : msg -> Msg
    , routingConfig : Route.Config Msg
    , db : JD.Value
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , networkId : NetworkId
    }


updateCtx : (msg -> Msg) -> Model -> UpdateContext msg
updateCtx toMsg model =
    let
        routingConfig =
            { ignoreMsg = \_ -> NoMsg
            , urlChangedMsg = UrlChanged
            }
    in
    { toMsg = toMsg
    , routingConfig = routingConfig
    , db = model.db
    , connectedWallet = model.connectedWallet
    , localStateUtxos = model.localStateUtxos
    , networkId = model.networkId
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ treasuryLoadingParamsForm } as model) =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        UrlChanged route ->
            let
                oldUrl =
                    model.appUrl

                newUrl =
                    Route.toAppUrl route
            in
            -- If the new URL is exactly the same, ignore
            if newUrl == oldUrl then
                ( model, Cmd.none )

            else
                handleUrlChange route model

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

        -- Signature page
        SignTxMsg pageMsg ->
            case model.page of
                SignTxPage previousPage pageModel ->
                    handleSignTxMsg previousPage pageModel pageMsg model

                _ ->
                    ( model, Cmd.none )

        -- Treasury management
        StartTreasurySetup ->
            ( model
            , Task.perform StartTreasurySetupWithCurrentTime Time.now
            )

        StartTreasurySetupWithCurrentTime currentTime ->
            ( { model | page = SetupPage <| Setup.init currentTime }, Cmd.none )

        SetupMsg pageMsg ->
            case model.page of
                SetupPage pageModel ->
                    Setup.update (updateCtx SetupMsg model) model.scripts pageMsg pageModel
                        |> (\newPageModel ->
                                case Setup.isDone newPageModel of
                                    Nothing ->
                                        ( { model | page = SetupPage newPageModel }, Cmd.none )

                                    Just loaded ->
                                        ( { model | page = TreasuryPage <| Treasury.init model.scripts loaded }, Cmd.none )
                           )

                _ ->
                    ( model, Cmd.none )

        StartTreasuryLoading ->
            case Loading.startLoading (updateCtx identity model) model.scripts model.treasuryLoadingParamsForm of
                Ok ( loading, { updatedLocalState, runTasks } ) ->
                    { model
                        | localStateUtxos = updatedLocalState
                        , page = LoadingPage loading
                    }
                        |> withTasks (List.map (ConcurrentTask.map TreasuryLoadingTask) runTasks)

                Err error ->
                    ( { model
                        | page = HomePage
                        , treasuryLoadingParamsForm = { treasuryLoadingParamsForm | error = Just error }
                      }
                    , Cmd.none
                    )

        LoadingParamsMsg paramsMsg ->
            ( { model | treasuryLoadingParamsForm = LoadingParams.updateForm paramsMsg treasuryLoadingParamsForm }, Cmd.none )

        TreasuryMsg pageMsg ->
            handleTreasuryMsg pageMsg model

        RefreshUtxos loaded ->
            let
                ( loading, { updatedLocalState, runTasks } ) =
                    Loading.refreshUtxos (updateCtx identity model) loaded
            in
            { model
                | localStateUtxos = updatedLocalState
                , page = LoadingPage loading
            }
                |> withTasks (List.map (ConcurrentTask.map TreasuryLoadingTask) runTasks)

        -- Task port
        OnTaskProgress ( taskPool, cmd ) ->
            ( { model | taskPool = taskPool }, cmd )

        OnTaskComplete taskCompleted ->
            handleTask taskCompleted model


handleTreasuryMsg : Treasury.Msg -> Model -> ( Model, Cmd Msg )
handleTreasuryMsg pageMsg model =
    case model.page of
        TreasuryPage pageModel ->
            Treasury.update (updateCtx TreasuryMsg model) pageMsg pageModel
                |> Tuple.mapFirst (\newPageModel -> { model | page = TreasuryPage newPageModel })

        _ ->
            ( model, Cmd.none )



-- URL changes


handleUrlChange : Route -> Model -> ( Model, Cmd Msg )
handleUrlChange route model =
    let
        appUrl =
            Route.toAppUrl route

        pushUrlCmd =
            if appUrl == model.appUrl then
                Cmd.none

            else
                pushUrl <| AppUrl.toString appUrl
    in
    case route of
        Route.NotFound ->
            Debug.todo "Handle 404 page"

        Route.Home ->
            ( { model
                | error = Nothing
                , page = HomePage
                , appUrl = appUrl
              }
            , pushUrlCmd
            )

        Route.Signing args ->
            let
                subject =
                    SignTx.Unknown

                prep =
                    Maybe.map makeSigningPrep args.tx

                makeSigningPrep tx =
                    { tx = tx
                    , txId = Transaction.computeTxId tx
                    , expectedSignatures = List.map .keyHash args.expectedSigners
                    , signerDescriptions =
                        List.map (\s -> ( s.keyHash, s.keyName )) args.expectedSigners
                            |> Bytes.Map.fromList
                    }
            in
            ( { model
                | error = Nothing
                , page = SignTxPage { previousPage = model.page } <| SignTx.initialModel subject prep
                , appUrl = appUrl
              }
            , pushUrlCmd
            )



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

        -- The wallet just signed a Tx
        Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeyWitnesses)) ->
            case model.page of
                SignTxPage previousPage pageModel ->
                    ( { model | page = SignTxPage previousPage <| SignTx.addWalletSignatures vkeyWitnesses pageModel }
                    , Cmd.none
                    )

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)) ->
            case model.page of
                SignTxPage { previousPage } pageModel ->
                    let
                        ( updatedPreviousPage, updatedLocalState ) =
                            updatePageWithTx (SignTx.getTxInfo pageModel) model.localStateUtxos previousPage
                    in
                    ( { model
                        | page = SignTxPage { previousPage = updatedPreviousPage } <| SignTx.recordSubmittedTx txId pageModel
                        , localStateUtxos = updatedLocalState
                      }
                    , Cmd.none
                    )

                -- No other page expects to submit a Tx
                _ ->
                    ( model, Cmd.none )

        Ok (Cip30.ApiResponse _ _) ->
            ( { model | error = Just "Unhandled CIP30 response yet" }
            , Cmd.none
            )

        -- Received an error message from the wallet
        Ok (Cip30.ApiError { info, code }) ->
            ( { model
                | page = resetSigningStep info model.page
                , error = Just <| "Wallet Error (code " ++ String.fromInt code ++ "):\n" ++ info
              }
            , Cmd.none
            )

        -- Unknown type of message received from the wallet
        Ok (Cip30.UnhandledResponseType error) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        Err error ->
            ( { model | error = Just <| "CIP-30 decoding error: " ++ JD.errorToString error }
            , Cmd.none
            )


updatePageWithTx : Maybe { txId : Bytes TransactionId, tx : Transaction } -> Utxo.RefDict Output -> Page -> ( Page, Utxo.RefDict Output )
updatePageWithTx maybeTx localStateUtxos page =
    case maybeTx of
        Just { txId, tx } ->
            let
                { updatedState } =
                    TxIntent.updateLocalState txId tx localStateUtxos
            in
            case page of
                HomePage ->
                    ( page, updatedState )

                SetupPage pageModel ->
                    ( SetupPage <| Setup.updateWithTx txId pageModel, updatedState )

                LoadingPage _ ->
                    ( page, updatedState )

                TreasuryPage pageModel ->
                    ( TreasuryPage <| Treasury.updateWithTx txId pageModel, updatedState )

                SignTxPage _ _ ->
                    ( page, updatedState )

        Nothing ->
            ( page, localStateUtxos )


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SignTxPage previousPage pageModel ->
            SignTxPage previousPage <| SignTx.resetSubmission error pageModel

        _ ->
            page



-- Signature page


handleSignTxMsg : { previousPage : Page } -> SignTx.Model -> SignTx.Msg -> Model -> ( Model, Cmd Msg )
handleSignTxMsg previousPage pageModel msg model =
    let
        ( walletSignTx, walletSubmitTx ) =
            case model.connectedWallet of
                Nothing ->
                    ( \_ -> Cmd.none
                    , \_ -> Cmd.none
                    )

                Just wallet ->
                    ( \tx -> toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = True } tx))
                    , \tx -> toWallet (Cip30.encodeRequest (Cip30.submitTx wallet tx))
                    )

        ctx =
            { wrapMsg = SignTxMsg
            , wallet = model.connectedWallet
            , walletSignTx = walletSignTx
            , walletSubmitTx = walletSubmitTx
            }
    in
    SignTx.update ctx msg pageModel
        |> Tuple.mapFirst (\newPageModel -> { model | page = SignTxPage previousPage newPageModel })



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
    case ( taskCompleted, model.page ) of
        ( ReadLoadingParams (Just paramsForm), _ ) ->
            ( { model | treasuryLoadingParamsForm = paramsForm }, Cmd.none )

        ( TreasuryLoadingTask subTask, LoadingPage loading ) ->
            handleCompletedLoadingTask subTask loading model

        _ ->
            ( model, Cmd.none )


handleCompletedLoadingTask : Loading.TaskCompleted -> Loading -> Model -> ( Model, Cmd Msg )
handleCompletedLoadingTask task loading ({ treasuryLoadingParamsForm } as model) =
    let
        encounteredLoadingError error =
            ( { model
                | page = HomePage
                , treasuryLoadingParamsForm = { treasuryLoadingParamsForm | error = Just error }
              }
            , Cmd.none
            )
    in
    case Loading.updateWithCompletedTask (updateCtx identity model) model.scripts task loading of
        Ok ( updatedLoadingTreasury, { updatedLocalState, runTasks } ) ->
            case Loading.upgradeIfTreasuryLoadingFinished updatedLocalState updatedLoadingTreasury of
                Just ( loadedTreasury, loadedLocalState ) ->
                    case Loading.doubleCheckTreasuryScriptsHashes loadedTreasury of
                        Ok _ ->
                            { model
                                | page = TreasuryPage <| Treasury.init model.scripts loadedTreasury
                                , localStateUtxos = loadedLocalState
                            }
                                |> withTasks (List.map (ConcurrentTask.map TreasuryLoadingTask) runTasks)

                        Err error ->
                            encounteredLoadingError error

                Nothing ->
                    { model
                        | page = LoadingPage updatedLoadingTreasury
                        , localStateUtxos = updatedLocalState
                    }
                        |> withTasks (List.map (ConcurrentTask.map TreasuryLoadingTask) runTasks)

        Err error ->
            encounteredLoadingError error


withTasks : List (ConcurrentTask String TaskCompleted) -> Model -> ( Model, Cmd Msg )
withTasks tasks model =
    let
        ( updatedTaskPool, tasksCmds ) =
            ConcurrentTask.Extra.attemptEach
                { pool = model.taskPool
                , send = sendTask
                , onComplete = OnTaskComplete
                }
                tasks
    in
    ( { model | taskPool = updatedTaskPool }
    , Cmd.batch tasksCmds
    )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    let
        ctx pageMsg =
            { toMsg = pageMsg
            , refreshUtxos = RefreshUtxos
            , routeConfig =
                { ignoreMsg = \_ -> NoMsg
                , urlChangedMsg = UrlChanged
                }
            , networkId = model.networkId
            , connectedWallet = model.connectedWallet
            }
    in
    div []
        [ Header.view { connect = ConnectButtonClicked, disconnect = DisconnectWallet } model.discoveredWallets model.connectedWallet
        , viewLocalStateUtxosSection model.localStateUtxos
        , case model.page of
            HomePage ->
                Page.Home.view
                    { startTreasurySetup = StartTreasurySetup
                    , startTreasuryLoading = StartTreasuryLoading
                    , loadingParamsMsg = LoadingParamsMsg
                    }
                    model.treasuryLoadingParamsForm

            SetupPage pageModel ->
                Setup.view (ctx SetupMsg) pageModel

            LoadingPage loading ->
                Loading.view loading

            TreasuryPage pageModel ->
                Treasury.view (ctx TreasuryMsg) pageModel

            SignTxPage _ pageModel ->
                let
                    viewContext =
                        { wrapMsg = SignTxMsg
                        , routingConfig =
                            { ignoreMsg = \_ -> NoMsg
                            , urlChangedMsg = UrlChanged
                            }
                        , wallet = model.connectedWallet
                        , networkId = model.networkId
                        }
                in
                SignTx.view viewContext pageModel
        , viewError model.error
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            Html.pre [] [ text <| "ERROR:\n" ++ error ]



-- Local state UTxOs


viewLocalStateUtxosSection : Utxo.RefDict Output -> Html msg
viewLocalStateUtxosSection utxos =
    Html.p [] [ text <| "Local state UTxOs size: " ++ String.fromInt (Dict.Any.size utxos) ]
