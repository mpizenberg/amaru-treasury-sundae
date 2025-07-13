port module Main exposing (main)

import Api exposing (ProtocolParams)
import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Script as Script exposing (NativeScript(..), PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction
import Cardano.TxIntent exposing (SpendSource(..))
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output)
import ConcurrentTask
import ConcurrentTask.Extra
import Dict
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra
import Page.SignTx as SignTx exposing (Subject(..))
import Platform.Cmd as Cmd
import Result.Extra
import Route exposing (Route)
import Storage
import TreasuryManagement exposing (ActionStatus(..), SetupFormMsg(..), TreasuryAction(..), TreasuryLoadingParamsForm, TreasuryManagement(..), TxState(..))
import TreasuryManagement.Setup


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
    | SignTxButtonClicked SignTx.Subject SignTx.Prep
      -- Signature page
    | SignTxMsg SignTx.Msg
      -- Treasury management
    | TreasuryManagementMsg TreasuryManagement.Msg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = ReadLoadingParams (Maybe TreasuryLoadingParamsForm)
    | LoadingParamsSaved
    | TreasuryManagementTask TreasuryManagement.TaskCompleted



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

    -- Treasury Management
    , treasuryManagementModel : TreasuryManagement.Model

    -- Errors
    , error : Maybe String
    }


initialModel : JD.Value -> TreasuryManagement.Setup.Scripts -> Int -> Model
initialModel db scripts posixTimeMs =
    let
        treasuryLoadingParams =
            { pragmaScriptHash = ""
            , registriesSeedUtxo = { transactionId = "", outputIndex = 0 }
            , treasuryConfigExpiration = posixTimeMs
            , error = Nothing
            }
    in
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = Home
    , appUrl = Route.toAppUrl Route.Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , treasuryManagementModel = TreasuryManagement.init scripts treasuryLoadingParams
    , error = Nothing
    }


type Page
    = Home
    | SignTxPage SignTx.Model


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
            Storage.read { db = db, storeName = "stuff" } TreasuryManagement.treasuryLoadingParamsFormDecoder { key = "treasuryLoadingParams" }
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


updateCtx : (TreasuryManagement.Msg -> Msg) -> Model -> TreasuryManagement.UpdateContext {} Msg
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
update msg model =
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

        SignTxButtonClicked signSubject signingPrep ->
            ( { model | page = SignTxPage <| SignTx.initialModel signSubject (Just signingPrep) }
            , Cmd.none
            )

        -- Signature page
        SignTxMsg pageMsg ->
            case model.page of
                SignTxPage pageModel ->
                    handleSignTxMsg pageModel pageMsg model

                _ ->
                    ( model, Cmd.none )

        -- Treasury management
        TreasuryManagementMsg pageMsg ->
            let
                ( pageModel, pageCmd, { updatedLocalState, runTasks } ) =
                    TreasuryManagement.update (updateCtx TreasuryManagementMsg model) pageMsg model.treasuryManagementModel

                ( updatedTaskPool, tasksCmds ) =
                    ConcurrentTask.Extra.attemptEach
                        { pool = model.taskPool
                        , send = sendTask
                        , onComplete = OnTaskComplete
                        }
                        (List.map (ConcurrentTask.map TreasuryManagementTask) runTasks)
            in
            ( { model
                | localStateUtxos = updatedLocalState
                , taskPool = updatedTaskPool
                , treasuryManagementModel = pageModel
              }
            , Cmd.batch (pageCmd :: tasksCmds)
            )

        -- Task port
        OnTaskProgress ( taskPool, cmd ) ->
            ( { model | taskPool = taskPool }, cmd )

        OnTaskComplete taskCompleted ->
            handleTask taskCompleted model



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
                , page = Home
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
                , page = SignTxPage <| SignTx.initialModel subject prep
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
                SignTxPage pageModel ->
                    ( { model | page = SignTxPage <| SignTx.addWalletSignatures vkeyWitnesses pageModel }
                    , Cmd.none
                    )

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)) ->
            case model.page of
                SignTxPage pageModel ->
                    let
                        ( updatedTreasuryManagementModel, { updatedLocalState } ) =
                            TreasuryManagement.updateWithTx model.localStateUtxos (SignTx.getTxInfo pageModel) model.treasuryManagementModel
                    in
                    ( { model
                        | page = SignTxPage <| SignTx.recordSubmittedTx txId pageModel
                        , localStateUtxos = updatedLocalState
                        , treasuryManagementModel = updatedTreasuryManagementModel
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


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SignTxPage pageModel ->
            SignTxPage <| SignTx.resetSubmission error pageModel

        _ ->
            page



-- Signature page


handleSignTxMsg : SignTx.Model -> SignTx.Msg -> Model -> ( Model, Cmd Msg )
handleSignTxMsg pageModel msg model =
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
        |> Tuple.mapFirst (\newPageModel -> { model | page = SignTxPage newPageModel })



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
    let
        ctx =
            updateCtx TreasuryManagementMsg model
    in
    case taskCompleted of
        ReadLoadingParams (Just paramsForm) ->
            ( { model | treasuryManagementModel = TreasuryManagement.setLoadingParamsForm paramsForm model.treasuryManagementModel }, Cmd.none )

        ReadLoadingParams Nothing ->
            ( model, Cmd.none )

        LoadingParamsSaved ->
            ( model, Cmd.none )

        TreasuryManagementTask subTask ->
            let
                ( subModel, subCmds, { updatedLocalState, runTasks } ) =
                    TreasuryManagement.handleCompletedTask ctx subTask model.treasuryManagementModel

                ( updatedTaskPool, tasksCmds ) =
                    ConcurrentTask.Extra.attemptEach
                        { pool = model.taskPool
                        , send = sendTask
                        , onComplete = OnTaskComplete
                        }
                        (List.map (ConcurrentTask.map TreasuryManagementTask) runTasks)
            in
            ( { model
                | localStateUtxos = updatedLocalState
                , taskPool = updatedTaskPool
                , treasuryManagementModel = subModel
              }
            , Cmd.batch (subCmds :: tasksCmds)
            )



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            viewHome model

        SignTxPage pageModel ->
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


viewHome : Model -> Html Msg
viewHome model =
    let
        treasuryViewContext =
            updateCtx TreasuryManagementMsg model
    in
    div []
        [ viewError model.error
        , viewWalletSection model
        , viewLocalStateUtxosSection model.localStateUtxos
        , TreasuryManagement.view treasuryViewContext model.treasuryManagementModel
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
            div [] [ walletIcon w, text <| walletDescription w ++ " ", connectButton w ]
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
