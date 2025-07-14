module Utils exposing (..)

import Html exposing (Html, text)
import Html.Attributes as HA
import Time exposing (Posix)


viewError : Maybe String -> Html msg
viewError maybeErrors =
    case maybeErrors of
        Nothing ->
            text ""

        Just errors ->
            Html.pre [ HA.style "color" "red" ] [ text errors ]


viewExpirationDate : Int -> Html msg
viewExpirationDate posixDate =
    Html.p [] [ text <| "Expiration date (Posix): " ++ String.fromInt posixDate ++ " (" ++ displayPosixDate (Time.millisToPosix posixDate) ++ ")" ]


displayPosixDate : Posix -> String
displayPosixDate posix =
    let
        year =
            Time.toYear Time.utc posix
                |> String.fromInt

        month =
            Time.toMonth Time.utc posix
                |> Debug.toString

        day =
            Time.toDay Time.utc posix
                |> String.fromInt

        hour =
            Time.toHour Time.utc posix
                |> String.fromInt
                |> String.padLeft 2 '0'

        minutes =
            Time.toMinute Time.utc posix
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    "UTC: " ++ year ++ " / " ++ month ++ " / " ++ day ++ " - " ++ hour ++ ":" ++ minutes


spinner : Html msg
spinner =
    Html.span [ HA.class "loader" ] []
