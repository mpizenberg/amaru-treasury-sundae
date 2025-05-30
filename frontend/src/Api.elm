module Api exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (NetworkId(..))
import Cardano.Gov exposing (CostModels)
import Cardano.Uplc as Uplc
import Cardano.Utxo exposing (TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http
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



-- Retrieve Tx


{-| Task to retrieve the raw CBOR of a given Tx.
It uses the Koios API, proxied through the app server (because CORS).
-}
taskRetrieveTx : NetworkId -> Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes a)
taskRetrieveTx networkId txId =
    let
        thisTxDecoder : Decoder (Bytes a)
        thisTxDecoder =
            koiosFirstTxBytesDecoder
                |> JD.andThen
                    (\tx ->
                        if tx.txId == txId then
                            JD.succeed tx.txCbor

                        else
                            JD.fail <| "The retrieved Tx (" ++ Bytes.toHex tx.txId ++ ") does not correspond to the expected one (" ++ Bytes.toHex txId ++ ")"
                    )
    in
    -- TODO: change to authenticated free tier Koios request
    ConcurrentTask.Http.post
        { url = "/proxy/json"
        , headers = []
        , body =
            ConcurrentTask.Http.jsonBody
                (JE.object
                    [ ( "url", JE.string <| koiosUrl networkId ++ "/tx_cbor" )
                    , ( "method", JE.string "POST" )
                    , ( "body", JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) [ txId ] ) ] )
                    ]
                )
        , expect = ConcurrentTask.Http.expectJson thisTxDecoder
        , timeout = Nothing
        }


koiosFirstTxBytesDecoder : Decoder { txId : Bytes TransactionId, txCbor : Bytes a }
koiosFirstTxBytesDecoder =
    let
        oneTxBytesDecoder =
            JD.map2 (\txId cbor -> { txId = txId, txCbor = cbor })
                (JD.field "tx_hash" Bytes.jsonDecoder)
                (JD.field "cbor" Bytes.jsonDecoder)
    in
    JD.list oneTxBytesDecoder
        |> JD.andThen
            (\txs ->
                case txs of
                    [] ->
                        JD.fail "The Tx was not found. Maybe the request was done for the wrong network?"

                    [ tx ] ->
                        JD.succeed tx

                    _ ->
                        JD.fail "The server unexpectedly returned more than 1 transaction."
            )
