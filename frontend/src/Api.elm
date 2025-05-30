module Api exposing (..)

import Cardano.Address exposing (NetworkId(..))
import Cardano.Gov exposing (CostModels)
import Cardano.Uplc as Uplc
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Natural as N exposing (Natural)


{-| Free Tier Koios API token.
Expiration date: 2026-02-17.
-}
koiosApiToken : String
koiosApiToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZGRyIjoic3Rha2UxdTl5dG5rMnB4dTlobHJnM2Rqc2t2ODhraG1uOTBsZGowMHE0Z3o4YTJycXN1NGc0NHB6Mm0iLCJleHAiOjE3NzEzMjA0MzQsInRpZXIiOjEsInByb2pJRCI6Imdvdi12b3RpbmcifQ.8PiNPWnc9j5mjL4e7pbGA3cXEB0GepXnxWjXn0mYocY"


koiosUrl : NetworkId -> String
koiosUrl networkId =
    case networkId of
        Testnet ->
            "https://preview.koios.rest/api/v1"

        Mainnet ->
            "https://api.koios.rest/api/v1"



-- Protocol Parameters


type alias ProtocolParams =
    { costModels : CostModels
    , registrationDeposit : Natural
    }


defaultProtocolParams : ProtocolParams
defaultProtocolParams =
    { costModels = Uplc.conwayDefaultCostModels
    , registrationDeposit = N.fromSafeInt <| 2 * 1000 * 1000 -- ₳2
    }


loadProtocolParams : NetworkId -> (Result Http.Error ProtocolParams -> msg) -> Cmd msg
loadProtocolParams networkId toMsg =
    Http.request
        { method = "POST"
        , url = koiosUrl networkId ++ "/ogmios"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "jsonrpc", JE.string "2.0" )
                    , ( "method", JE.string "queryLedgerState/protocolParameters" )
                    ]
                )
        , expect = Http.expectJson toMsg protocolParamsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 registrationDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , registrationDeposit = registrationDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "stakeCredentialDeposit", "ada", "lovelace" ] <| JD.map N.fromSafeInt JD.int)
