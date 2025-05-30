port module Main exposing (main)

import Api exposing (ProtocolParams)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.TxIntent exposing (TxIntent)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Dict exposing (Dict)
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Result.Extra
import Treasury exposing (SpendConfig)
import Types


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


port toWallet : JD.Value -> Cmd msg


port fromWallet : (JD.Value -> msg) -> Sub msg


type Msg
    = WalletMsg JD.Value
    | ConnectButtonClicked { id : String }
    | GotNetworkParams (Result Http.Error ProtocolParams)



-- MODEL


type alias Model =
    { networkId : NetworkId
    , protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , scripts : Scripts
    , treasuryManagement : TreasuryManagement
    , error : Maybe String
    }


initialModel : Scripts -> Model
initialModel scripts =
    { networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , scripts = scripts
    , treasuryManagement = TreasuryUnspecified
    , error = Nothing
    }


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , scopeOwner : PlutusScript
    , pragma : PlutusScript
    }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasuryFullyLoaded LoadedTreasury


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , scopes : Dict String Scope
    }


type alias Scope =
    { name : String
    , ownerScript : PlutusScript
    , sundaeTreasuryScript : PlutusScript
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , utxos : Utxo.RefDict Output
    }


init : { blueprints : List JD.Value } -> ( Model, Cmd Msg )
init { blueprints } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        ( scripts, error ) =
            case sundaeTreasuryBlueprint of
                Nothing ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopeOwner = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , pragma = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Just "Failed the retrieve the blueprint of the Sundae Treasury script"
                    )

                Just blueprint ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 blueprint.scriptBytes
                      , scopeOwner = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , pragma = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Nothing
                    )

        model =
            initialModel scripts
    in
    ( { model | error = error }
    , Cmd.batch
        [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
        , Api.loadProtocolParams model.networkId GotNetworkParams
        ]
    )


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
        WalletMsg value ->
            handleWalletMsg value model

        ConnectButtonClicked { id } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Just 3 })) )

        GotNetworkParams (Err httpError) ->
            ( { model | error = Just <| Debug.toString httpError }, Cmd.none )

        GotNetworkParams (Ok params) ->
            ( { model | protocolParams = Debug.log "params" params }, Cmd.none )


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

        _ ->
            ( model, Cmd.none )



-- Disburse


disburse : Scope -> OutputReference -> (Value -> List TxIntent) -> Value -> Result String (List TxIntent)
disburse scope utxoRef receivers value =
    case Dict.Any.get utxoRef scope.utxos of
        Nothing ->
            Err <| "The selected UTxO isn’t in the known list of UTxOs for this scope: " ++ Debug.toString utxoRef

        Just spentOutput ->
            let
                spendConfig : SpendConfig
                spendConfig =
                    { treasuryScriptBytes = Script.cborWrappedBytes scope.sundaeTreasuryScript
                    , requiredSigners = Debug.todo ""
                    , requiredWithdrawals = Debug.todo ""
                    , spentInputRef = utxoRef
                    , spentOutput = spentOutput
                    }

                overflowValue =
                    Value.subtract value spentOutput.amount
            in
            if overflowValue == Value.zero then
                Ok <| Treasury.disburse spendConfig receivers value

            else
                Err <| "Trying to disburse more than is available in this UTxO. Overflow value is: " ++ Debug.toString overflowValue



-- Scope owner


type ScopeOwnerAction
    = Standard
    | SwapOrder
    | SwapCancel


type ScopeOwner
    = ScopeLedger
    | ScopeConsensus
    | ScopeMerceneries
    | ScopeMarketing



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    div []
        [ viewWalletSection model
        , viewError model.error
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
        ]
