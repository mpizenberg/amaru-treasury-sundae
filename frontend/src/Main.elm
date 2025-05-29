port module Main exposing (..)

import Browser
import Bytes.Comparable as Bytes
import Cardano.Address as Address
import Cardano.Cip30 as Cip30
import Cardano.Gov exposing (CostModels)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output)
import Dict
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Value)
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
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


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


type Msg
    = WalletMsg Value
    | ConnectButtonClicked { id : String }



-- MODEL


type alias Model =
    { protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    }


initialModel : Model
initialModel =
    { protocolParams = defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    }


type alias ProtocolParams =
    { costModels : CostModels
    , registrationDeposit : Natural
    }


defaultProtocolParams : ProtocolParams
defaultProtocolParams =
    { costModels = Uplc.conwayDefaultCostModels
    , registrationDeposit = N.fromSafeInt <| 5 * 1000 * 1000 -- ₳5
    }


type TreasuryState
    = TreasurySelection
    | CreatingTreasury TreasuryCreationData
    | ManagingTreasury Treasury


type alias TreasuryCreationData =
    { treasuryId : String
    , scopes : List ScopeConfig
    }


type alias ScopeConfig =
    { name : String
    , expirationTime : Int -- Posix time
    , owner : String
    }


defaultScopeConfig : ScopeConfig
defaultScopeConfig =
    { name = ""
    , expirationTime = 0
    , owner = ""
    }


type alias Treasury =
    { id : String
    , scopes : List Scope
    }


type alias Scope =
    { name : String
    , config : ScopeConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WalletMsg value ->
            handleWalletMsg value model

        ConnectButtonClicked { id } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Just 3 })) )


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewWalletSection model
        ]



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
