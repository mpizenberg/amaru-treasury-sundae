module Treasury.SetupForm exposing (Msg(..), SetupForm, Validation, ViewContext, init, update, view)

import Bytes.Comparable as Bytes
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import MultisigScript exposing (MultisigScript)
import Time exposing (Posix)
import Treasury.Scopes as Scopes exposing (Scopes)
import Utils exposing (displayPosixDate, viewError)


type alias SetupForm =
    { ledgerOwner : String
    , consensusOwner : String
    , mercenariesOwner : String
    , marketingOwner : String
    , expiration : Int
    , validation : Maybe Validation
    }


type alias Validation =
    Result String { expiration : Int, scopeOwners : Scopes MultisigScript }


init : Posix -> SetupForm
init currentTime =
    { ledgerOwner = ""
    , consensusOwner = ""
    , mercenariesOwner = ""
    , marketingOwner = ""
    , expiration = Time.posixToMillis currentTime
    , validation = Nothing
    }


type Msg
    = LedgerOwnerField String
    | ConsensusOwnerField String
    | MercenariesOwnerField String
    | MarketingOwnerField String
    | ExpirationField String
    | ValidateSetupForm


update : Msg -> SetupForm -> SetupForm
update msg form =
    case msg of
        LedgerOwnerField value ->
            { form | ledgerOwner = value }

        ConsensusOwnerField value ->
            { form | consensusOwner = value }

        MercenariesOwnerField value ->
            { form | mercenariesOwner = value }

        MarketingOwnerField value ->
            { form | marketingOwner = value }

        ExpirationField value ->
            { form | expiration = Maybe.withDefault 0 <| String.toInt value }

        ValidateSetupForm ->
            { form | validation = Just <| validate form }


validate : SetupForm -> Validation
validate form =
    Scopes form.ledgerOwner form.consensusOwner form.mercenariesOwner form.marketingOwner
        |> Scopes.map validateKeyHash
        |> Scopes.toResult
        -- TODO: add some validation of the expiration Int
        |> Result.map (\scopes -> { expiration = form.expiration, scopeOwners = scopes })


validateKeyHash : String -> Result String MultisigScript
validateKeyHash hex =
    case Bytes.fromHex hex of
        Nothing ->
            Err <| "Invalid hex string: " ++ hex

        Just bytes ->
            if Bytes.width bytes /= 28 then
                Err <| "This credential is not 28 bytes long: " ++ Bytes.toHex bytes

            else
                Ok (MultisigScript.Signature bytes)



-- View ##############################################################


type alias ViewContext msg =
    { toMsg : Msg -> msg
    , tryBuildSetupTxs : msg
    }


view : ViewContext msg -> SetupForm -> Html msg
view ctx form =
    case form.validation of
        Nothing ->
            viewTreasurySetupFormHelper ctx form Nothing

        Just (Err errors) ->
            viewTreasurySetupFormHelper ctx form (Just errors)

        Just (Ok { expiration, scopeOwners }) ->
            viewScopeOwnersSetup ctx expiration scopeOwners


viewTreasurySetupFormHelper : ViewContext msg -> SetupForm -> Maybe String -> Html msg
viewTreasurySetupFormHelper ctx form errors =
    div []
        [ Html.h2 [] [ text "Setup New Treasury" ]
        , Html.p []
            [ Html.label [] [ text "Expiration date (Posix): " ]
            , Html.input
                [ HA.type_ "number"
                , HA.value <| String.fromInt form.expiration
                , HE.onInput (ctx.toMsg << ExpirationField)
                ]
                []
            , text <| " (" ++ displayPosixDate (Time.millisToPosix form.expiration) ++ ")"
            ]
        , div []
            [ Html.h3 [] [ text "Scope Owners" ]
            , Html.p [] [ text "Enter the aicone multisigs for each scope owner (for now just enter a public key hash):" ]
            ]
        , Html.map ctx.toMsg <|
            div []
                [ viewFormField "Ledger" form.ledgerOwner LedgerOwnerField
                , viewFormField "Consensus" form.consensusOwner ConsensusOwnerField
                , viewFormField "Mercenaries" form.mercenariesOwner MercenariesOwnerField
                , viewFormField "Marketing" form.marketingOwner MarketingOwnerField
                ]
        , Html.p []
            [ Html.button [ onClick (ctx.toMsg ValidateSetupForm) ]
                [ text "Validate scope owners & prepare Txs" ]
            ]
        , viewError errors
        ]


viewFormField : String -> String -> (String -> Msg) -> Html Msg
viewFormField label value fieldMsg =
    div []
        [ Html.label [] [ text <| label ++ ": " ]
        , Html.input
            [ HA.type_ "text"
            , HA.placeholder "Enter public key hash"
            , HE.onInput fieldMsg
            , HA.value value
            ]
            []
        ]


viewScopeOwnersSetup : ViewContext msg -> Int -> Scopes MultisigScript -> Html msg
viewScopeOwnersSetup ctx expiration scopeOwners =
    div []
        [ Html.h2 [] [ text "Setup New Treasury" ]
        , Html.p [] [ text <| "Expiration date (Posix): " ++ String.fromInt expiration ++ " (" ++ displayPosixDate (Time.millisToPosix expiration) ++ ")" ]
        , Html.h3 [] [ text "Scope Owners" ]
        , Html.ul []
            [ Html.li [] [ text <| "ledger: " ++ Debug.toString scopeOwners.ledger ]
            , Html.li [] [ text <| "consensus: " ++ Debug.toString scopeOwners.consensus ]
            , Html.li [] [ text <| "mercenaries: " ++ Debug.toString scopeOwners.mercenaries ]
            , Html.li [] [ text <| "marketing: " ++ Debug.toString scopeOwners.marketing ]
            ]
        , Html.p []
            [ Html.button [ onClick ctx.tryBuildSetupTxs ]
                [ text "Build the Txs" ]
            ]
        ]
