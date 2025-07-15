module Header exposing (Context, view)

import Cardano.Address as Address
import Cardano.Cip30 as Cip30
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)


type alias Context msg =
    { connect : { id : String } -> msg
    , disconnect : msg
    }


view : Context msg -> List Cip30.WalletDescriptor -> Maybe Cip30.Wallet -> Html msg
view ctx discoveredWallets connectedWallet =
    viewWalletSection ctx discoveredWallets connectedWallet


viewWalletSection : Context msg -> List Cip30.WalletDescriptor -> Maybe Cip30.Wallet -> Html msg
viewWalletSection ctx discoveredWallets connectedWallet =
    case connectedWallet of
        Nothing ->
            div []
                [ div [] [ text "CIP-30 wallets detected:" ]
                , viewAvailableWallets ctx discoveredWallets
                ]

        Just wallet ->
            viewConnectedWallet ctx wallet


viewAvailableWallets : Context msg -> List Cip30.WalletDescriptor -> Html msg
viewAvailableWallets ctx wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        connectButton { id } =
            Html.button [ onClick (ctx.connect { id = id }) ] [ text "connect" ]

        walletRow w =
            div [] [ walletIcon w, text <| walletDescription w ++ " ", connectButton w ]
    in
    div [] (List.map walletRow wallets)


viewConnectedWallet : Context msg -> Cip30.Wallet -> Html msg
viewConnectedWallet ctx wallet =
    div []
        [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
        , div [] [ text <| "Address: " ++ (Address.toBech32 <| Cip30.walletChangeAddress wallet) ]
        , Html.button [ onClick ctx.disconnect ] [ text "Disconnect" ]
        ]
