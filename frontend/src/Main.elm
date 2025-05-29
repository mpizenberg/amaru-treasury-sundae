port module Main exposing (..)

import Browser
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
    | StartTreasuryCreation
    | StartTreasuryReopening
    | UpdateTreasuryId String
    | AddScope
    | UpdateScopeName Int String
    | RemoveScope Int
    | CreateTreasury
    | BackToTreasurySelection



-- MODEL


type alias Model =
    { protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , treasuryState : TreasuryState
    }


initialModel : Model
initialModel =
    { protocolParams = defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , treasuryState = TreasurySelection
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

        StartTreasuryCreation ->
            ( { model | treasuryState = CreatingTreasury { treasuryId = "", scopes = [ { name = "" } ] } }, Cmd.none )

        StartTreasuryReopening ->
            -- TODO: Implement treasury reopening with networking
            Debug.todo "Implement treasury reopening with networking"

        UpdateTreasuryId newId ->
            case model.treasuryState of
                CreatingTreasury data ->
                    ( { model | treasuryState = CreatingTreasury { data | treasuryId = newId } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddScope ->
            case model.treasuryState of
                CreatingTreasury data ->
                    let
                        newScopes =
                            data.scopes ++ [ { name = "" } ]
                    in
                    ( { model | treasuryState = CreatingTreasury { data | scopes = newScopes } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateScopeName index newName ->
            case model.treasuryState of
                CreatingTreasury data ->
                    let
                        updateScope i scope =
                            if i == index then
                                { scope | name = newName }

                            else
                                scope

                        newScopes =
                            List.indexedMap updateScope data.scopes
                    in
                    ( { model | treasuryState = CreatingTreasury { data | scopes = newScopes } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RemoveScope index ->
            case model.treasuryState of
                CreatingTreasury data ->
                    let
                        newScopes =
                            List.take index data.scopes ++ List.drop (index + 1) data.scopes

                        -- Ensure we always have at least one scope
                        finalScopes =
                            if List.isEmpty newScopes then
                                [ { name = "" } ]

                            else
                                newScopes
                    in
                    ( { model | treasuryState = CreatingTreasury { data | scopes = finalScopes } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CreateTreasury ->
            case model.treasuryState of
                CreatingTreasury data ->
                    let
                        -- Convert scope configs to actual scopes
                        scopes =
                            List.map (\config -> { name = config.name, config = config }) data.scopes

                        treasury =
                            { id = data.treasuryId, scopes = scopes }
                    in
                    ( { model | treasuryState = ManagingTreasury treasury }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackToTreasurySelection ->
            ( { model | treasuryState = TreasurySelection }, Cmd.none )


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
        [ case model.connectedWallet of
            Nothing ->
                viewWalletSection model

            Just wallet ->
                div []
                    [ viewConnectedWallet wallet
                    , viewTreasurySection model
                    ]
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



-- Treasury


viewTreasurySection : Model -> Html Msg
viewTreasurySection model =
    case model.treasuryState of
        TreasurySelection ->
            viewTreasurySelection

        CreatingTreasury data ->
            viewTreasuryCreation data

        ManagingTreasury treasury ->
            viewTreasuryManagement treasury


viewTreasurySelection : Html Msg
viewTreasurySelection =
    div []
        [ Html.h2 [] [ text "Treasury Management" ]
        , div []
            [ Html.button [ onClick StartTreasuryCreation ] [ text "Create New Treasury" ]
            , Html.button [ onClick StartTreasuryReopening ] [ text "Re-open Existing Treasury" ]
            ]
        ]


viewTreasuryCreation : TreasuryCreationData -> Html Msg
viewTreasuryCreation data =
    let
        canCreate =
            not (String.isEmpty data.treasuryId) && List.all (\scope -> not (String.isEmpty scope.name)) data.scopes

        scopeRow index scope =
            div []
                [ Html.input
                    [ Html.Attributes.placeholder "Scope name"
                    , Html.Attributes.value scope.name
                    , Html.Events.onInput (UpdateScopeName index)
                    ]
                    []
                , if List.length data.scopes > 1 then
                    Html.button [ onClick (RemoveScope index) ] [ text "Remove" ]

                  else
                    text ""
                ]
    in
    div []
        [ Html.h2 [] [ text "Create New Treasury" ]
        , div []
            [ Html.label [] [ text "Treasury ID:" ]
            , Html.input
                [ Html.Attributes.placeholder "Enter unique treasury identifier"
                , Html.Attributes.value data.treasuryId
                , Html.Events.onInput UpdateTreasuryId
                ]
                []
            ]
        , Html.h3 [] [ text "Scopes" ]
        , div [] (List.indexedMap scopeRow data.scopes)
        , Html.button [ onClick AddScope ] [ text "Add Scope" ]
        , div []
            [ Html.button
                [ onClick CreateTreasury
                , Html.Attributes.disabled (not canCreate)
                ]
                [ text "Create Treasury" ]
            , Html.button [ onClick BackToTreasurySelection ] [ text "Cancel" ]
            ]
        ]


viewTreasuryManagement : Treasury -> Html Msg
viewTreasuryManagement treasury =
    div []
        [ Html.h2 [] [ text ("Managing Treasury: " ++ treasury.id) ]
        , Html.h3 [] [ text "Scopes:" ]
        , div [] (List.map viewScope treasury.scopes)
        , Html.button [ onClick BackToTreasurySelection ] [ text "Back to Treasury Selection" ]
        ]


viewScope : Scope -> Html Msg
viewScope scope =
    div []
        [ Html.strong [] [ text scope.name ]
        , text " - Configuration: (to be implemented)"
        ]
