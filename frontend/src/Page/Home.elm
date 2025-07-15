module Page.Home exposing (Context, view)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Treasury.LoadingParams as LoadingParams
import Utils


type alias Context a msg =
    { a
        | startTreasurySetup : msg
        , startTreasuryLoading : msg
        , loadingParamsMsg : LoadingParams.Msg -> msg
    }


view : Context a msg -> LoadingParams.Form -> Html msg
view ctx form =
    div []
        [ Html.h2 [] [ Html.text "Setup a new treasury" ]
        , Html.button [ onClick ctx.startTreasurySetup ]
            [ text "Setup a new treasury" ]
        , Html.h2 [] [ Html.text "Load an existing treasury" ]
        , Html.p [] [ text "With the following parameters:" ]
        , Html.map ctx.loadingParamsMsg <|
            LoadingParams.viewForm form
        , Html.button [ onClick ctx.startTreasuryLoading ]
            [ text "Load treasury" ]
        , Utils.viewError form.error
        ]
