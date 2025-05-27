port module Main exposing (..)

import Browser
import Cardano.Address as Address
import Cardano.Cip30 as Cip30
import Cardano.Utxo exposing (Output, OutputReference)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JDecode exposing (Value)
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


type Model
    = Startup
    | WalletDiscovered (List Cip30.WalletDescriptor)
    | WalletLoading Cip30.Wallet
    | WalletLoaded
        { wallet : Cip30.Wallet
        , utxos : List ( OutputReference, Output )
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- UPDATE


walletResponseDecoder : JDecode.Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.singleton 30 Cip30.apiDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WalletMsg value, _ ) ->
            case ( JDecode.decodeValue walletResponseDecoder value, model ) of
                -- We just discovered available wallets
                ( Ok (Cip30.AvailableWallets wallets), Startup ) ->
                    ( WalletDiscovered wallets, Cmd.none )

                -- We just connected to the wallet, let’s ask for the available utxos
                ( Ok (Cip30.EnabledWallet wallet), WalletDiscovered _ ) ->
                    ( WalletLoading wallet
                    , toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    )

                -- We just received the utxos
                ( Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)), WalletLoading wallet ) ->
                    ( WalletLoaded { wallet = wallet, utxos = utxos }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered _ ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Nothing })) )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Startup ->
            div [] [ div [] [ text "Hello Cardano!" ] ]

        WalletDiscovered availableWallets ->
            div []
                [ div [] [ text "Hello Cardano!" ]
                , div [] [ text "CIP-30 wallets detected:" ]
                , viewAvailableWallets availableWallets
                ]

        WalletLoading _ ->
            div [] [ text "Loading wallet assets ..." ]

        WalletLoaded { wallet, utxos } ->
            div []
                [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
                , div [] [ text <| "Address: " ++ (Address.toBech32 <| Cip30.walletChangeAddress wallet) ]
                , div [] [ text <| "UTxO count: " ++ String.fromInt (List.length utxos) ]
                ]


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
