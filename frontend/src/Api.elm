module Api exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (NetworkId(..))
import Cardano.Gov exposing (CostModels)
import Cardano.MultiAsset exposing (AssetName, MultiAsset, PolicyId)
import Cardano.Transaction as Transaction
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import ConcurrentTask.Http
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra
import Natural as N exposing (Natural)
import Result.Extra
import Storage


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



-- Retrieve all UTxOs associated with the provided tokens
-- TODO: return -> MultiAsset (Utxo.RefDict Output)


retrieveAssetsUtxos : NetworkId -> List ( Bytes PolicyId, Bytes AssetName ) -> ConcurrentTask String (MultiAsset (Utxo.RefDict Output))
retrieveAssetsUtxos networkId assets =
    Debug.todo ""



--
--
-- Retrieve NFT UTxO


retrieveAssetUtxo : NetworkId -> Bytes PolicyId -> Bytes AssetName -> ConcurrentTask ConcurrentTask.Http.Error OutputReference
retrieveAssetUtxo networkId policyId assetName =
    let
        encodeAsset ( id, name ) =
            JE.list identity
                [ JE.string <| Bytes.toHex id
                , JE.string <| Bytes.toHex name
                ]
    in
    ConcurrentTask.Http.post
        { url = koiosUrl networkId ++ "/asset_utxos"
        , headers = [ ConcurrentTask.Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
        , body =
            ConcurrentTask.Http.jsonBody <|
                JE.object [ ( "_asset_list", JE.list encodeAsset [ ( policyId, assetName ) ] ) ]
        , expect = ConcurrentTask.Http.expectJson koiosLiveUtxoDecoder
        , timeout = Nothing
        }


koiosLiveUtxoDecoder : Decoder OutputReference
koiosLiveUtxoDecoder =
    JD.list koiosUtxoRefDecoder
        |> JD.map
            (List.filterMap
                (\{ txId, index, isSpent } ->
                    if isSpent then
                        Nothing

                    else
                        Just <| OutputReference txId index
                )
            )
        |> JD.andThen
            (\refs ->
                case refs of
                    [] ->
                        JD.fail "The UTxO was not found. Maybe the request was done for the wrong network?"

                    [ ref ] ->
                        JD.succeed ref

                    _ ->
                        JD.fail "The server unexpectedly returned more than 1 UTxO."
            )


koiosUtxoRefDecoder : Decoder { txId : Bytes TransactionId, index : Int, isSpent : Bool }
koiosUtxoRefDecoder =
    JD.map3 (\txId index isSpent -> { txId = txId, index = index, isSpent = isSpent })
        (JD.field "tx_hash" Bytes.jsonDecoder)
        (JD.field "tx_index" JD.int)
        (JD.field "is_spent" JD.bool)



-- Retrieve Output from the OutputReference


retrieveOutput : NetworkId -> OutputReference -> ConcurrentTask String ( OutputReference, Output )
retrieveOutput networkId utxo =
    retrieveTxBytes networkId utxo.transactionId
        |> ConcurrentTask.mapError Debug.toString
        |> ConcurrentTask.andThen
            (\txBytes ->
                case Transaction.deserialize txBytes of
                    Nothing ->
                        ConcurrentTask.fail <| "Failed to deserialize the Tx bytes: " ++ Bytes.toHex txBytes

                    Just tx ->
                        List.Extra.getAt utxo.outputIndex tx.body.outputs
                            |> Maybe.map (Tuple.pair utxo)
                            |> Result.fromMaybe "Missing output in Tx"
                            |> ConcurrentTask.fromResult
            )



-- Retrieve Txs


{-| Task to retrieve the raw CBOR of multiple Txs with a single request.
For Txs already retrieved, load them from the cache.
-}
retrieveAllTxsBytesWithCache : { db : JD.Value } -> NetworkId -> List (Bytes TransactionId) -> ConcurrentTask String (List { txId : Bytes TransactionId, txCbor : Bytes a })
retrieveAllTxsBytesWithCache db networkId txIds =
    let
        loadTxsFromCache : ConcurrentTask x { success : List { txId : Bytes TransactionId, txCbor : Bytes a }, notFoundInCache : List (Bytes TransactionId) }
        loadTxsFromCache =
            List.map (loadTxCborFromCache db) txIds
                |> ConcurrentTask.Extra.parallel
                |> ConcurrentTask.map splitNotFoundInCache

        splitNotFoundInCache : List (Result x (Bytes a)) -> { success : List { txId : Bytes TransactionId, txCbor : Bytes a }, notFoundInCache : List (Bytes TransactionId) }
        splitNotFoundInCache results =
            let
                pairs : List (Result (Bytes TransactionId) { txId : Bytes TransactionId, txCbor : Bytes a })
                pairs =
                    List.map2
                        (\id cborResult ->
                            Result.Extra.mapBoth
                                (\_ -> id)
                                (\cbor -> { txId = id, txCbor = cbor })
                                cborResult
                        )
                        txIds
                        results

                ( success, notFoundInCache ) =
                    Result.Extra.partition pairs
            in
            { success = success, notFoundInCache = notFoundInCache }

        writeTxsToCache : List { txId : Bytes TransactionId, txCbor : Bytes a } -> ConcurrentTask x ()
        writeTxsToCache txs =
            List.map (\{ txId, txCbor } -> writeTxCborToCache db txId txCbor) txs
                |> ConcurrentTask.batch
                |> ConcurrentTask.map (always ())
    in
    loadTxsFromCache
        |> ConcurrentTask.andThen
            (\{ success, notFoundInCache } ->
                -- Make a request for all not found
                -- and aggregate with those from cache
                retrieveAllTxsBytes networkId notFoundInCache
                    |> ConcurrentTask.mapError Debug.toString
                    |> ConcurrentTask.andThen
                        (\txs ->
                            writeTxsToCache txs
                                |> ConcurrentTask.andThenDo (ConcurrentTask.succeed <| success ++ txs)
                        )
            )


{-| Task to retrieve the raw CBOR of multiple Txs with a single request.
-}
retrieveAllTxsBytes : NetworkId -> List (Bytes TransactionId) -> ConcurrentTask ConcurrentTask.Http.Error (List { txId : Bytes TransactionId, txCbor : Bytes a })
retrieveAllTxsBytes networkId txIds =
    ConcurrentTask.Http.post
        { url = koiosUrl networkId ++ "/tx_cbor"
        , headers = [ ConcurrentTask.Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
        , body =
            ConcurrentTask.Http.jsonBody <|
                JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) txIds ) ]
        , expect = ConcurrentTask.Http.expectJson (JD.list oneTxBytesDecoder)
        , timeout = Nothing
        }


{-| Task to retrieve the raw CBOR of a given Tx.
-}
retrieveTxBytes : NetworkId -> Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes a)
retrieveTxBytes networkId txId =
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
    ConcurrentTask.Http.post
        { url = koiosUrl networkId ++ "/tx_cbor"
        , headers = [ ConcurrentTask.Http.header "Authorization" <| "Bearer " ++ koiosApiToken ]
        , body =
            ConcurrentTask.Http.jsonBody <|
                JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) [ txId ] ) ]
        , expect = ConcurrentTask.Http.expectJson thisTxDecoder
        , timeout = Nothing
        }


koiosFirstTxBytesDecoder : Decoder { txId : Bytes TransactionId, txCbor : Bytes a }
koiosFirstTxBytesDecoder =
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


oneTxBytesDecoder : Decoder { txId : Bytes TransactionId, txCbor : Bytes a }
oneTxBytesDecoder =
    JD.map2 (\txId cbor -> { txId = txId, txCbor = cbor })
        (JD.field "tx_hash" Bytes.jsonDecoder)
        (JD.field "cbor" Bytes.jsonDecoder)



-- Cache Tx


loadTxCborFromCache : { db : JD.Value } -> Bytes TransactionId -> ConcurrentTask String (Bytes cbor)
loadTxCborFromCache { db } txId =
    Storage.read { db = db, storeName = "tx" } Bytes.jsonDecoder { key = Bytes.toHex txId }


writeTxCborToCache : { db : JD.Value } -> Bytes TransactionId -> Bytes cbor -> ConcurrentTask x ()
writeTxCborToCache { db } txId txCbor =
    Storage.write { db = db, storeName = "tx" } Bytes.jsonEncode { key = Bytes.toHex txId } txCbor
