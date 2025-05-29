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
    | StartTreasuryCreation
    | StartTreasuryReopening
    | UpdateTreasuryId String
    | AddScope
    | UpdateScopeName Int String
    | UpdateScopeExpiration Int String
    | EditPermissionScript Int PermissionType String
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
    , expirationTime : Maybe Int -- Posix time
    , permissions : ScopePermissions
    }


defaultScopeConfig : ScopeConfig
defaultScopeConfig =
    { name = ""
    , expirationTime = Nothing
    , permissions = defaultScopePermissions
    }


type alias ScopePermissions =
    { disburse : MultisigScript
    , reorganize : MultisigScript
    , sweep : MultisigScript
    }


defaultScopePermissions : ScopePermissions
defaultScopePermissions =
    { disburse = MultisigScript.AnyOf []
    , reorganize = MultisigScript.AnyOf []
    , sweep = MultisigScript.AnyOf []
    }


type PermissionType
    = DisbursePermission
    | ReorganizePermission
    | SweepPermission


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
            ( { model | treasuryState = CreatingTreasury { treasuryId = "", scopes = [ defaultScopeConfig ] } }, Cmd.none )

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
                            data.scopes ++ [ defaultScopeConfig ]
                    in
                    ( { model | treasuryState = CreatingTreasury { data | scopes = newScopes } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateScopeName index newName ->
            updateScopeConfig index (\scope -> { scope | name = newName }) model

        UpdateScopeExpiration index expirationStr ->
            let
                expirationTime =
                    if String.isEmpty expirationStr then
                        Nothing

                    else
                        String.toInt expirationStr
            in
            updateScopeConfig index (\scope -> { scope | expirationTime = expirationTime }) model

        EditPermissionScript index permissionType scriptStr ->
            case model.treasuryState of
                CreatingTreasury _ ->
                    let
                        updatePermissions scope =
                            let
                                permissions =
                                    scope.permissions

                                newScript =
                                    MultisigScript.Signature <| Bytes.fromHexUnchecked scriptStr

                                newPermissions =
                                    case permissionType of
                                        DisbursePermission ->
                                            { permissions | disburse = newScript }

                                        ReorganizePermission ->
                                            { permissions | reorganize = newScript }

                                        SweepPermission ->
                                            { permissions | sweep = newScript }
                            in
                            { scope | permissions = newPermissions }
                    in
                    updateScopeConfig index updatePermissions model

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
                                [ defaultScopeConfig ]

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
                        -- Validate that all permissions are properly configured
                        isValidScope scope =
                            not (String.isEmpty scope.name)
                                && isValidPermissions scope.permissions

                        isValidPermissions permissions =
                            isValidScript permissions.disburse
                                && isValidScript permissions.reorganize
                                && isValidScript permissions.sweep

                        isValidScript script =
                            case script of
                                _ ->
                                    -- TODO
                                    True

                        -- Other script types are considered valid for now
                        allScopesValid =
                            List.all isValidScope data.scopes

                        treasuryIdValid =
                            not (String.isEmpty data.treasuryId)
                    in
                    if treasuryIdValid && allScopesValid then
                        let
                            scopes =
                                List.map (\config -> { name = config.name, config = config }) data.scopes

                            treasury =
                                { id = data.treasuryId, scopes = scopes }
                        in
                        ( { model | treasuryState = ManagingTreasury treasury }, Cmd.none )

                    else
                        ( model, Cmd.none )

                -- Could add error handling here
                _ ->
                    ( model, Cmd.none )

        BackToTreasurySelection ->
            ( { model | treasuryState = TreasurySelection }, Cmd.none )


updateScopeConfig : Int -> (ScopeConfig -> ScopeConfig) -> Model -> ( Model, Cmd Msg )
updateScopeConfig index updateFn model =
    case model.treasuryState of
        CreatingTreasury data ->
            let
                updateScope i scope =
                    if i == index then
                        updateFn scope

                    else
                        scope

                newScopes =
                    List.indexedMap updateScope data.scopes
            in
            ( { model | treasuryState = CreatingTreasury { data | scopes = newScopes } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
        -- Enhanced validation - all permissions must be filled
        isValidScope scope =
            not (String.isEmpty scope.name)
                && isValidPermissions scope.permissions

        isValidPermissions permissions =
            isValidScript permissions.disburse
                && isValidScript permissions.reorganize
                && isValidScript permissions.sweep

        isValidScript script =
            case script of
                _ ->
                    -- TODO
                    True

        -- Other script types are considered valid for now
        canCreate =
            not (String.isEmpty data.treasuryId)
                && List.all isValidScope data.scopes

        scopeRow index scope =
            div [ Html.Attributes.style "border" "1px solid #ccc", Html.Attributes.style "padding" "10px", Html.Attributes.style "margin" "10px 0" ]
                [ -- Scope name
                  div []
                    [ Html.label [] [ text "Scope Name:" ]
                    , Html.input
                        [ Html.Attributes.placeholder "Scope name"
                        , Html.Attributes.value scope.name
                        , Html.Events.onInput (UpdateScopeName index)
                        ]
                        []
                    ]

                -- Expiration time
                , div []
                    [ Html.label [] [ text "Expiration Time (POSIX):" ]
                    , Html.input
                        [ Html.Attributes.placeholder "e.g., 1703980800"
                        , Html.Attributes.value (Maybe.map String.fromInt scope.expirationTime |> Maybe.withDefault "")
                        , Html.Events.onInput (UpdateScopeExpiration index)
                        , Html.Attributes.type_ "number"
                        ]
                        []
                    ]

                -- Permissions (all required)
                , Html.h4 [] [ text "Permissions (all required):" ]
                , viewPermissionEditor index "Disburse" DisbursePermission scope.permissions.disburse
                , viewPermissionEditor index "Reorganize" ReorganizePermission scope.permissions.reorganize
                , viewPermissionEditor index "Sweep" SweepPermission scope.permissions.sweep

                -- Remove scope button
                , if List.length data.scopes > 1 then
                    Html.button [ onClick (RemoveScope index) ] [ text "Remove Scope" ]

                  else
                    text ""
                ]

        viewPermissionEditor : Int -> String -> PermissionType -> MultisigScript -> Html Msg
        viewPermissionEditor scopeIndex permissionName permissionType script =
            div [ Html.Attributes.style "margin" "10px 0" ]
                [ Html.label [] [ text (permissionName ++ ":") ]
                , viewScriptEditor scopeIndex permissionType script
                ]

        viewScriptEditor : Int -> PermissionType -> MultisigScript -> Html Msg
        viewScriptEditor scopeIndex permissionType script =
            case script of
                MultisigScript.Signature credentialHash ->
                    div [ Html.Attributes.style "margin-left" "10px" ]
                        [ Html.input
                            [ Html.Attributes.placeholder "Enter credential hash (required)"
                            , Html.Attributes.value <| Bytes.toHex credentialHash
                            , Html.Events.onInput (EditPermissionScript scopeIndex permissionType)
                            , Html.Attributes.style "width" "400px"
                            ]
                            []
                        ]

                _ ->
                    div [ Html.Attributes.style "margin-left" "10px" ]
                        [ text "Complex script editing not yet implemented" ]
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
        , Html.h3 [] [ text "Scopes Configuration" ]
        , div [] (List.indexedMap scopeRow data.scopes)
        , Html.button [ onClick AddScope ] [ text "Add Scope" ]
        , div []
            [ Html.button
                [ onClick CreateTreasury
                , Html.Attributes.disabled (not canCreate)
                , Html.Attributes.title
                    (if canCreate then
                        "Create Treasury"

                     else
                        "Please fill all required fields"
                    )
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
        , div [] (List.map viewScopeDetails treasury.scopes)
        , Html.button [ onClick BackToTreasurySelection ] [ text "Back to Treasury Selection" ]
        ]


viewScopeDetails : Scope -> Html Msg
viewScopeDetails scope =
    div [ Html.Attributes.style "border" "1px solid #ccc", Html.Attributes.style "padding" "10px", Html.Attributes.style "margin" "10px 0" ]
        [ Html.h4 [] [ text scope.name ]
        , div [] [ text ("Expiration: " ++ (Maybe.map String.fromInt scope.config.expirationTime |> Maybe.withDefault "Never")) ]
        , Html.h5 [] [ text "Permissions:" ]
        , viewPermissionStatus "Disburse" scope.config.permissions.disburse
        , viewPermissionStatus "Reorganize" scope.config.permissions.reorganize
        , viewPermissionStatus "Sweep" scope.config.permissions.sweep
        ]


viewPermissionStatus : String -> MultisigScript -> Html Msg
viewPermissionStatus name script =
    div []
        [ text (name ++ ": ")
        , case script of
            MultisigScript.Signature credHash ->
                text ("Signature required from " ++ Bytes.toHex credHash)

            _ ->
                text "Complex script configured"
        ]
