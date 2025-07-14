module Treasury.LoadingParams exposing (Form, LoadingParams, Msg(..), encode, formDecoder, updateForm, validate, view, viewForm, viewReload)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Utxo exposing (OutputReference)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Time exposing (Posix)
import Utils exposing (displayPosixDate, viewExpirationDate)


type alias Form =
    { pragmaScriptHash : String
    , registriesSeedUtxo : { transactionId : String, outputIndex : Int }
    , treasuryConfigExpiration : Int
    , error : Maybe String
    }


formDecoder : JD.Decoder Form
formDecoder =
    JD.map4 Form
        (JD.field "pragmaScriptHash" JD.string)
        (JD.field "registriesSeedUtxo"
            (JD.map2 (\a b -> { transactionId = a, outputIndex = b })
                (JD.field "transactionId" JD.string)
                (JD.field "outputIndex" JD.int)
            )
        )
        (JD.field "treasuryConfigExpiration" JD.int)
        (JD.succeed Nothing)


type alias LoadingParams =
    { pragmaScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : OutputReference
    , expiration : Posix
    }


encode : LoadingParams -> JE.Value
encode { pragmaScriptHash, registriesSeedUtxo, expiration } =
    JE.object
        [ ( "pragmaScriptHash", JE.string <| Bytes.toHex pragmaScriptHash )
        , ( "registriesSeedUtxo"
          , JE.object
                [ ( "transactionId", JE.string <| Bytes.toHex registriesSeedUtxo.transactionId )
                , ( "outputIndex", JE.int registriesSeedUtxo.outputIndex )
                ]
          )
        , ( "treasuryConfigExpiration", JE.int <| Time.posixToMillis expiration )
        ]



-- UPDATE ############################################################


type Msg
    = UpdatePragmaScriptHash String
    | UpdateRegistriesSeedTransactionId String
    | UpdateRegistriesSeedOutputIndex String
    | UpdateExpiration String


updateForm : Msg -> Form -> Form
updateForm msg form =
    let
        currentUtxo =
            form.registriesSeedUtxo
    in
    case msg of
        UpdatePragmaScriptHash value ->
            { form | pragmaScriptHash = value, error = Nothing }

        UpdateRegistriesSeedTransactionId value ->
            { form | registriesSeedUtxo = { currentUtxo | transactionId = value }, error = Nothing }

        UpdateRegistriesSeedOutputIndex value ->
            case String.toInt value of
                Just outputIndex ->
                    { form | registriesSeedUtxo = { currentUtxo | outputIndex = outputIndex }, error = Nothing }

                Nothing ->
                    form

        UpdateExpiration value ->
            case String.toInt value of
                Just expiration ->
                    { form | treasuryConfigExpiration = expiration }

                Nothing ->
                    form


validate : Form -> Result String LoadingParams
validate formParams =
    case ( Bytes.fromHex formParams.pragmaScriptHash, Bytes.fromHex formParams.registriesSeedUtxo.transactionId ) of
        ( Nothing, _ ) ->
            Err <| "Pragma script hash is not a valid hex string: " ++ formParams.pragmaScriptHash

        ( _, Nothing ) ->
            Err <| "Registries seed utxo transaction id is not a valid hex string: " ++ formParams.registriesSeedUtxo.transactionId

        ( Just scriptHash, Just transactionId ) ->
            if Bytes.width scriptHash /= 28 then
                Err <| "Pragma script hash is " ++ String.fromInt (Bytes.width scriptHash) ++ " bytes long, but expected 28"

            else if Bytes.width transactionId /= 32 then
                Err <| "Registries seed utxo transaction id is " ++ String.fromInt (Bytes.width transactionId) ++ " bytes long, but expected 32"

            else
                Ok
                    { pragmaScriptHash = scriptHash
                    , registriesSeedUtxo = OutputReference transactionId formParams.registriesSeedUtxo.outputIndex
                    , expiration = Time.millisToPosix formParams.treasuryConfigExpiration
                    }



-- VIEW ##############################################################


viewForm : Form -> Html Msg
viewForm form =
    div []
        [ Html.p []
            [ Html.label [] [ text "Pragma Scopes script hash: " ]
            , Html.input
                [ HA.type_ "text"
                , HA.placeholder "44dd0678ba5f89b41869362ea3d3e509f94e48bd57be57faedaad0c6"
                , HA.value form.pragmaScriptHash
                , HE.onInput UpdatePragmaScriptHash
                ]
                []
            ]
        , Html.p []
            [ Html.label [] [ text "Registries Seed UTxO - Tx ID: " ]
            , Html.input
                [ HA.type_ "text"
                , HA.placeholder "9737488cbd45bc71d5c12490865b20ec8aa9f74b3110f4c2aa588b5e1e09dad6"
                , HA.value form.registriesSeedUtxo.transactionId
                , HE.onInput UpdateRegistriesSeedTransactionId
                ]
                []
            , Html.label [] [ text " - Output Index: # " ]
            , Html.input
                [ HA.type_ "number"
                , HA.size 2
                , HA.min "0"
                , HA.value <| String.fromInt form.registriesSeedUtxo.outputIndex
                , HE.onInput UpdateRegistriesSeedOutputIndex
                ]
                []
            ]
        , Html.p []
            [ Html.label [] [ text "Expiration date (Posix): " ]
            , Html.input
                [ HA.type_ "number"
                , HA.value <| String.fromInt form.treasuryConfigExpiration
                , HE.onInput UpdateExpiration
                ]
                []
            , text <| " (" ++ displayPosixDate (Time.millisToPosix form.treasuryConfigExpiration) ++ ")"
            ]
        ]


viewReload : LoadingParams -> Html msg
viewReload { pragmaScriptHash, registriesSeedUtxo, expiration } =
    Html.div [ HA.style "padding" "16px", HA.style "box-shadow" "0 0 16px rgba(0, 0, 0, 0.2)" ]
        [ Html.p [] [ Html.strong [] [ text "KEEP this to be able to reload the Treasury:" ] ]
        , viewPragmaScopesScriptHash pragmaScriptHash
        , viewRegistriesSeedUtxo registriesSeedUtxo
        , viewExpirationDate <| Time.posixToMillis expiration
        ]


view : LoadingParams -> Html msg
view { pragmaScriptHash, registriesSeedUtxo, expiration } =
    div []
        [ viewPragmaScopesScriptHash pragmaScriptHash
        , viewRegistriesSeedUtxo registriesSeedUtxo
        , viewExpirationDate <| Time.posixToMillis expiration
        ]


viewPragmaScopesScriptHash : Bytes CredentialHash -> Html msg
viewPragmaScopesScriptHash scriptHash =
    Html.p [] [ text <| "(PRAGMA) Scopes script hash: " ++ Bytes.toHex scriptHash ]


viewRegistriesSeedUtxo : OutputReference -> Html msg
viewRegistriesSeedUtxo { transactionId, outputIndex } =
    Html.p [] [ text <| "Registries Seed UTXO: " ++ Bytes.toHex transactionId ++ " #" ++ String.fromInt outputIndex ]
