module Route exposing (Config, Route(..), fromLocationHref, inAppLink, toAppUrl)

import AppUrl exposing (AppUrl)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (OutputReference)
import Cbor.Decode
import Cbor.Encode
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing (Decoder)
import Time
import Treasury.LoadingParams exposing (LoadingParams)
import Url


type Route
    = Home
    | Setup
    | Treasury LoadingParams
    | Signing
        { tx : Maybe Transaction
        , expectedSigners : List { keyHash : Bytes CredentialHash, keyName : String }
        }
    | NotFound


type alias Config msg =
    { ignoreMsg : Route -> msg
    , urlChangedMsg : Route -> msg
    }


inAppLink : Config msg -> Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
inAppLink config route attrs children =
    Html.a
        (HE.preventDefaultOn "click" (linkClickDecoder config route)
            :: HA.href (AppUrl.toString <| toAppUrl <| route)
            :: attrs
        )
        children


linkClickDecoder : Config msg -> Route -> Decoder ( msg, Bool )
linkClickDecoder { ignoreMsg, urlChangedMsg } route =
    -- Custom decoder on link clicks to not overwrite expected behaviors for click with modifiers.
    -- For example, Ctrl+Click should open in a new tab, and Shift+Click in a new window.
    JD.map4
        (\ctrl meta shift wheel ->
            if ctrl || meta || shift || wheel /= 0 then
                ( ignoreMsg route, False )

            else
                ( urlChangedMsg route, True )
        )
        (JD.field "ctrlKey" JD.bool)
        (JD.field "metaKey" JD.bool)
        (JD.field "shiftKey" JD.bool)
        (JD.field "button" JD.int)


toAppUrl : Route -> AppUrl
toAppUrl route =
    case route of
        NotFound ->
            AppUrl.fromPath [ "page", "404" ]

        Home ->
            AppUrl.fromPath []

        Setup ->
            AppUrl.fromPath [ "page", "setup" ]

        Treasury { pragmaScriptHash, registriesSeedUtxo, expiration } ->
            { path = [ "page", "treasury" ]
            , queryParameters =
                Dict.fromList
                    [ ( "pragmaScriptHash", [ Bytes.toHex pragmaScriptHash ] )
                    , ( "registriesSeedUtxo", [ utxoRefToHex registriesSeedUtxo ] )
                    , ( "expiration", [ String.fromInt <| Time.posixToMillis expiration ] )
                    ]
            , fragment = Nothing
            }

        Signing { tx, expectedSigners } ->
            { path = [ "page", "signing" ]
            , queryParameters =
                Dict.fromList
                    [ ( "signer"
                      , List.map (\{ keyHash, keyName } -> Bytes.toHex keyHash ++ ";" ++ keyName) expectedSigners
                      )
                    ]
            , fragment = Maybe.map (Bytes.toHex << Transaction.serialize) tx
            }


utxoRefToHex : OutputReference -> String
utxoRefToHex ref =
    Utxo.encodeOutputReference ref
        |> Cbor.Encode.encode
        |> Bytes.fromBytes
        |> Bytes.toHex


utxoRefFromHex : String -> Maybe OutputReference
utxoRefFromHex hex =
    Bytes.fromHex hex
        |> Maybe.andThen (\b -> Bytes.toBytes b |> Cbor.Decode.decode Utxo.decodeOutputReference)


fromLocationHref : String -> Route
fromLocationHref locationHref =
    case Url.fromString locationHref |> Maybe.map AppUrl.fromUrl of
        Nothing ->
            NotFound

        Just { path, queryParameters, fragment } ->
            case path of
                [] ->
                    Home

                [ "page", "setup" ] ->
                    Setup

                [ "page", "treasury" ] ->
                    let
                        qParam name =
                            Dict.get name queryParameters
                                |> Maybe.andThen List.head

                        maybePragmaScriptHash =
                            qParam "pragmaScriptHash"
                                |> Maybe.andThen Bytes.fromHex

                        maybeRegistriesSeedUtxo =
                            qParam "registriesSeedUtxo"
                                |> Maybe.andThen utxoRefFromHex

                        maybeExpiration =
                            qParam "expiration"
                                |> Maybe.andThen String.toInt
                                |> Maybe.map Time.millisToPosix
                    in
                    case ( maybePragmaScriptHash, maybeRegistriesSeedUtxo, maybeExpiration ) of
                        ( Just pragmaScriptHash, Just registriesSeedUtxo, Just expiration ) ->
                            Treasury
                                { pragmaScriptHash = pragmaScriptHash
                                , registriesSeedUtxo = registriesSeedUtxo
                                , expiration = expiration
                                }

                        _ ->
                            Home

                [ "page", "signing" ] ->
                    Signing
                        { tx =
                            Maybe.andThen Bytes.fromHex fragment
                                |> Maybe.andThen Transaction.deserialize
                        , expectedSigners =
                            Dict.get "signer" queryParameters
                                |> Maybe.withDefault []
                                |> List.filterMap
                                    (\stringKeySigner ->
                                        case String.split ";" stringKeySigner of
                                            [ keyHashHex, keyName ] ->
                                                Bytes.fromHex keyHashHex
                                                    |> Maybe.map (\keyHash -> { keyHash = keyHash, keyName = keyName })

                                            _ ->
                                                Nothing
                                    )
                        }

                _ ->
                    NotFound
