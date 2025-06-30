module Page.SignTx exposing (Model, Msg, Prep, State, Subject(..), UpdateContext, ViewContext, addWalletSignatures, getTxInfo, initialModel, recordSubmittedTx, resetSubmission, update, view)

{-| This module handles the signing process for Cardano transactions, particularly
focusing on complex scenarios like Native or Plutus script multi-signatures.

Key design features:

  - Supports both connected wallet signing and file-based signature collection
  - Tracks expected signers
  - Allows transaction submission even with partial signatures (useful for M-of-N schemes)
  - Handles signature deduplication and verification against expected signers

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo exposing (TransactionId)
import File exposing (File)
import File.Select
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import Set
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


{-| Represents the two possible states of the signing page:

  - MissingTx: No transaction loaded yet
  - LoadedTx: Active transaction being processed for signatures

-}
type Model
    = MissingTx
    | LoadedTx State


{-| Full state of the signing page.
-}
type alias State =
    { subject : Subject
    , prep : Prep
    , vkeyWitnesses : BytesMap CredentialHash VKeyWitness
    , txWasSubmitted : Bool
    , error : Maybe String
    }


{-| Valuable information for preparation of Tx signing.
-}
type alias Prep =
    { txId : Bytes TransactionId
    , tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    , signerDescriptions : BytesMap CredentialHash String
    }


{-| Subject of the transaction being signed.
-}
type Subject
    = Unknown
    | TreasurySetup
    | MergeUtxos
    | PublishScript (Bytes CredentialHash)


initialModel : Subject -> Maybe Prep -> Model
initialModel subject maybePrep =
    case maybePrep of
        Just prep ->
            LoadedTx
                { subject = subject
                , prep = prep

                -- TODO: extract potentially already signed credentials
                , vkeyWitnesses = Bytes.Map.empty
                , txWasSubmitted = False
                , error = Nothing
                }

        Nothing ->
            MissingTx


getTxInfo : Model -> Maybe { tx : Transaction, txId : Bytes TransactionId }
getTxInfo model =
    case model of
        LoadedTx { prep } ->
            Just { tx = prep.tx, txId = prep.txId }

        _ ->
            Nothing



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | SignTxButtonClicked
    | LoadSignedTxButtonClicked
    | SignedTxFileSelected File
    | LoadedSignedTxJson String
    | SubmitTxButtonClicked


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , walletSignTx : Transaction -> Cmd msg
    , walletSubmitTx : Transaction -> Cmd msg
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case ( msg, model ) of
        ( SignTxButtonClicked, LoadedTx { prep } ) ->
            ( model
            , ctx.walletSignTx prep.tx
            )

        ( SignTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( LoadSignedTxButtonClicked, LoadedTx _ ) ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [] SignedTxFileSelected
            )

        ( LoadSignedTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( SignedTxFileSelected file, LoadedTx _ ) ->
            ( model
            , Task.attempt handleSignedTxFileRead (File.toString file)
                |> Cmd.map ctx.wrapMsg
            )

        ( SignedTxFileSelected _, _ ) ->
            ( model, Cmd.none )

        ( LoadedSignedTxJson txJsonStr, LoadedTx state ) ->
            case extractVkeyWitnesses state.prep.txId txJsonStr of
                Ok sigs ->
                    ( addWalletSignatures sigs model, Cmd.none )

                Err error ->
                    ( LoadedTx { state | error = Just error }, Cmd.none )

        ( LoadedSignedTxJson _, _ ) ->
            ( model, Cmd.none )

        ( SubmitTxButtonClicked, LoadedTx { prep, vkeyWitnesses } ) ->
            let
                signedTx =
                    Transaction.updateSignatures (\_ -> Just <| Bytes.Map.values vkeyWitnesses) prep.tx
            in
            ( model
            , ctx.walletSubmitTx signedTx
            )

        ( SubmitTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( NoMsg, _ ) ->
            ( model, Cmd.none )


handleSignedTxFileRead : Result x String -> Msg
handleSignedTxFileRead result =
    case result of
        Err _ ->
            NoMsg

        Ok signedTxJson ->
            LoadedSignedTxJson signedTxJson


{-| Extracts VKey witnesses from a serialized transaction JSON, performing several validations:

1.  Ensures the transaction can be properly decoded
2.  Verifies the transaction ID matches the expected one
3.  Returns the list of witnesses if all checks pass

-}
extractVkeyWitnesses : Bytes TransactionId -> String -> Result String (List VKeyWitness)
extractVkeyWitnesses txId rawJson =
    let
        txJsonDecoder =
            JD.field "cborHex" JD.string
                |> JD.andThen
                    (\txHex ->
                        case Bytes.fromHex txHex |> Maybe.andThen Transaction.deserialize of
                            Just tx ->
                                JD.succeed tx

                            Nothing ->
                                JD.fail <| "Unable to decode Tx hex: " ++ txHex
                    )
    in
    case JD.decodeString txJsonDecoder rawJson of
        Ok tx ->
            let
                decodedTxId =
                    Transaction.computeTxId tx
            in
            if decodedTxId /= txId then
                Err <| "The wrong Tx was uploaded. It has a Tx ID of " ++ Bytes.toHex decodedTxId ++ " instead of " ++ Bytes.toHex txId

            else
                Ok <| Maybe.withDefault [] tx.witnessSet.vkeywitness

        Err error ->
            Err <| "Error while decoding the Tx: " ++ JD.errorToString error


{-| Adds new signatures to the model, with important filtering logic:

  - Only accepts signatures from expected signers
  - Handles cases where wallets might provide more signatures than needed
  - Updates existing signatures for the same key hash

-}
addWalletSignatures : List VKeyWitness -> Model -> Model
addWalletSignatures newVkeyWitnesses model =
    case model of
        LoadedTx state ->
            let
                -- Filter out unexpected signatures.
                -- Because sometimes, a wallet will provide more signatures than strictly needed.
                -- For example for some 1-of-n native multisig, a wallet might provide multiple signatures
                -- even if we only want 1, and only paid fees for one.
                expectedVkeyWitnesses =
                    List.filter keyHashIsExpected newVkeyWitnesses

                keyHashIsExpected vkeyWitness =
                    List.member (Transaction.hashVKey vkeyWitness.vkey) state.prep.expectedSignatures

                updatedWitnesses =
                    List.foldl (\w acc -> Bytes.Map.insert (Transaction.hashVKey w.vkey) w acc)
                        state.vkeyWitnesses
                        expectedVkeyWitnesses
            in
            LoadedTx { state | vkeyWitnesses = updatedWitnesses }

        _ ->
            model


recordSubmittedTx : Bytes TransactionId -> Model -> Model
recordSubmittedTx txId model =
    case model of
        LoadedTx state ->
            if state.prep.txId == txId then
                LoadedTx { state | txWasSubmitted = True }

            else
                LoadedTx { state | error = Just "Tx ID mismatch with the submitted Tx" }

        _ ->
            model


resetSubmission : String -> Model -> Model
resetSubmission error model =
    case model of
        MissingTx ->
            MissingTx

        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | error = Just error, txWasSubmitted = False }



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , toCallbackPage : Subject -> msg
    , wallet : Maybe Cip30.Wallet
    , networkId : NetworkId
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div []
        [ Html.h2 [] [ text "Signing the Transaction" ]
        , Html.p [] [ text "This page facilitates complex signature processes, such as Native or Plutus scripts multi-sig." ]
        , case model of
            MissingTx ->
                Html.p [] [ text "No transaction loaded. Please go back to the preparation page and create a transaction first." ]

            LoadedTx { subject, prep, vkeyWitnesses, txWasSubmitted, error } ->
                let
                    sectionTitle title =
                        Html.h3 [] [ text title ]

                    gatheredSignaturesSection =
                        div []
                            [ div [] (viewExpectedSignatures prep vkeyWitnesses)
                            , Html.p [] [ text "At least one of these signatures is to pay the transaction fees." ]
                            ]

                    signSection =
                        let
                            downloadButton label description fileName someTx =
                                Html.a
                                    [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode (txUnsignedJson description someTx)
                                    , HA.download fileName
                                    ]
                                    [ Html.button [ onClick <| ctx.wrapMsg NoMsg ] [ text label ] ]

                            txUnsignedJson description someTx =
                                JE.encode 2 <|
                                    JE.object
                                        [ ( "type", JE.string "Tx ConwayEra" )
                                        , ( "description", JE.string description )
                                        , ( "cborHex", JE.string <| Bytes.toHex <| Transaction.serialize someTx )
                                        ]
                        in
                        div []
                            [ Html.h4 [] [ text "Sign Transaction" ]
                            , div []
                                [ if ctx.wallet == Nothing then
                                    Html.p [] [ text "If you want to sign with your web wallet, you need to connect it (see the page top)." ]

                                  else if Bytes.Map.isEmpty vkeyWitnesses then
                                    Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SignTxButtonClicked ] [ text "Sign with connected wallet" ] ]

                                  else
                                    let
                                        signedTx =
                                            Transaction.updateSignatures (\_ -> Just <| Bytes.Map.values vkeyWitnesses) prep.tx
                                    in
                                    Html.div []
                                        [ Html.button [ onClick <| ctx.wrapMsg SignTxButtonClicked ] [ text "Sign with connected wallet" ]
                                        , downloadButton "Download partially signed Tx" "signed" "tx-signed.json" signedTx
                                        ]
                                , Html.p [] [ text "If additional signatures are required, ask the relevant parties to partially sign the transaction and use the button below to load their signatures. You can share this page URL with them—it contains all transaction details they need for signing on their device." ]
                                , Html.div []
                                    [ downloadButton "Download unsigned Tx" "unsigned" "tx-unsigned.json" prep.tx
                                    , Html.button [ onClick <| ctx.wrapMsg LoadSignedTxButtonClicked ] [ text "Load signed Tx file" ]
                                    ]
                                ]
                            ]

                    submissionSection hasAllSignatures =
                        div []
                            [ Html.p []
                                [ if hasAllSignatures then
                                    text "All required signatures have been collected."

                                  else if List.isEmpty prep.expectedSignatures then
                                    text "Expected signers are unknown. Submit when you believe the transaction is ready."

                                  else
                                    text "Not all expected signatures are gathered yet, but you can still submit if ready."
                                ]
                            , if hasAllSignatures || List.isEmpty prep.expectedSignatures then
                                Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Transaction" ]

                              else
                                Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Transaction Anyway" ]
                            ]

                    successSection =
                        div []
                            [ Html.p [] [ text "✓ Transaction submitted successfully!" ]
                            , Html.p [] [ text <| "Transaction ID: " ++ Bytes.toHex prep.txId ]
                            , Html.p []
                                [ text "Track your transaction: "
                                , let
                                    cardanoScanBaseUrl =
                                        case ctx.networkId of
                                            Mainnet ->
                                                "https://cardanoscan.io/transaction/"

                                            Testnet ->
                                                "https://preview.cardanoscan.io/transaction/"
                                  in
                                  Html.a
                                    [ HA.href <| cardanoScanBaseUrl ++ Bytes.toHex prep.txId
                                    , HA.target "_blank"
                                    ]
                                    [ text <| "View on CardanoScan ↗" ]
                                ]
                            ]

                    callbackButton =
                        Html.p [] [ Html.button [ onClick <| ctx.toCallbackPage subject ] [ text <| "Back to page: " ++ callbackPageName ] ]

                    callbackPageName =
                        case subject of
                            Unknown ->
                                "unknown?"

                            TreasurySetup ->
                                "Treasury setup"

                            MergeUtxos ->
                                "Merge UTxOs"

                            PublishScript _ ->
                                "Treasury management"
                in
                div []
                    [ div []
                        [ Html.p [] [ text <| "Subject: " ++ Debug.toString subject ]
                        , Html.p [] [ text <| "Transaction ID: " ++ Bytes.toHex prep.txId ]
                        , Html.p [] [ text "Transaction details: (₳ amounts are in lovelaces)" ]
                        , Html.pre [] [ text <| prettyTx prep.tx ]
                        ]
                    , if List.isEmpty prep.expectedSignatures then
                        div []
                            [ sectionTitle "Gathered Signatures"
                            , gatheredSignaturesSection
                            , signSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection True
                            ]

                      else if
                        Set.isEmpty
                            (Set.diff
                                (Set.fromList <| List.map Bytes.toHex prep.expectedSignatures)
                                (Set.fromList <| List.map Bytes.toHex <| Bytes.Map.keys vkeyWitnesses)
                            )
                      then
                        div []
                            [ sectionTitle "Expected Signatures"
                            , gatheredSignaturesSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection True
                            ]

                      else
                        div []
                            [ sectionTitle "Expected Signatures"
                            , gatheredSignaturesSection
                            , signSection
                            , sectionTitle "Transaction Submission"
                            , submissionSection False
                            ]
                    , if txWasSubmitted then
                        successSection

                      else
                        text ""
                    , callbackButton
                    , viewError error
                    ]
        ]


viewExpectedSignatures : Prep -> BytesMap CredentialHash VKeyWitness -> List (Html msg)
viewExpectedSignatures prep vkeyWitnesses =
    let
        viewExpectedSigner : Bytes CredentialHash -> Html msg
        viewExpectedSigner keyHash =
            let
                keyName =
                    Bytes.Map.get keyHash prep.signerDescriptions
                        |> Maybe.withDefault "Unknown"
            in
            case Bytes.Map.get keyHash vkeyWitnesses of
                Just witness ->
                    Html.p []
                        [ Html.div [] [ text <| "✓ " ++ keyName ++ ": " ++ shortenedHex 8 (Bytes.toHex keyHash) ]
                        , Html.div [] [ text <| "VKey: " ++ shortenedHex 8 (Bytes.toHex witness.vkey) ]
                        , Html.div [] [ text <| "Signature: " ++ shortenedHex 8 (Bytes.toHex witness.signature) ]
                        ]

                Nothing ->
                    Html.p [] [ text <| "□ " ++ keyName ++ ": " ++ shortenedHex 8 (Bytes.toHex keyHash) ]
    in
    List.map viewExpectedSigner prep.expectedSignatures


viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            Html.div [ HA.style "background-color" "#FEF2F2" ]
                [ Html.p [] [ text "Error" ]
                , Html.pre [] [ text err ]
                ]


{-| Shorten some string, by only keeping the first and last few characters.
-}
shortenedHex : Int -> String -> String
shortenedHex visibleChars str =
    let
        strLength =
            String.length str
    in
    if strLength <= visibleChars * 2 then
        str

    else
        String.slice 0 visibleChars str
            ++ "..."
            ++ String.slice (strLength - visibleChars) strLength str
