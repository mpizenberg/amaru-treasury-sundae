module Treasury.Disburse exposing (BuildContext, Form, Msg(..), ViewContext, buildTx, disburse, initForm, update, viewForm)

import Bytes.Comparable exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Script as Script
import Cardano.TxIntent as TxIntent exposing (TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Time exposing (Posix)
import Treasury.Scope exposing (Scope, viewDetailedUtxo)
import Treasury.Sundae exposing (SpendConfig)
import Utils exposing (viewError)


type alias Form =
    { selectedUtxo : ( OutputReference, Output )
    , selectedScopeOwners : List ( String, MultisigScript, Bool )
    , recipients : List { address : String, value : Natural }
    , error : Maybe String
    }


initForm : String -> List ( String, MultisigScript ) -> ( OutputReference, Output ) -> Form
initForm scopeName allOwners spendingUtxo =
    let
        defaultOwnerSection =
            case scopeName of
                "contingency" ->
                    List.map (\( name, multisig ) -> ( name, multisig, True )) allOwners

                _ ->
                    List.map (\( name, multisig ) -> ( name, multisig, name == scopeName )) allOwners
    in
    { selectedUtxo = spendingUtxo
    , selectedScopeOwners = defaultOwnerSection
    , recipients = [ { address = "", value = (Tuple.second spendingUtxo).amount.lovelace } ]
    , error = Nothing
    }



-- UPDATE ############################################################


type Msg
    = CheckOwner String Bool
    | AddRecipient
    | RemoveRecipient Int
    | SetRecipientAddress Int String
    | SetRecipientValue Int String


update : Msg -> Form -> Form
update msg ({ selectedScopeOwners, recipients } as form) =
    case msg of
        CheckOwner scopeOwnerName isChecked ->
            let
                updatedSelection =
                    List.Extra.updateIf (\( name, _, _ ) -> name == scopeOwnerName)
                        (\( name, multisig, _ ) -> ( name, multisig, isChecked ))
                        selectedScopeOwners
            in
            { form | selectedScopeOwners = updatedSelection }

        AddRecipient ->
            { form | recipients = { address = "", value = N.zero } :: recipients }

        RemoveRecipient index ->
            { form | recipients = List.Extra.removeAt index recipients }

        SetRecipientAddress index address ->
            let
                updatedRecipients =
                    List.Extra.updateAt index
                        (\recipient -> { recipient | address = address })
                        recipients
            in
            { form | recipients = updatedRecipients, error = Nothing }

        SetRecipientValue index value ->
            case N.fromString value of
                Just okValue ->
                    let
                        updatedRecipients =
                            List.Extra.updateAt index
                                (\recipient -> { recipient | value = okValue })
                                form.recipients
                    in
                    { form | recipients = updatedRecipients, error = Nothing }

                Nothing ->
                    { form | error = Just <| "Invalid value: " ++ value }


type alias BuildContext a =
    { a
        | localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
        , wallet : Cip30.Wallet
    }


buildTx : BuildContext a -> OutputReference -> Posix -> Scope -> Form -> Result String TxFinalized
buildTx { localStateUtxos, networkId, wallet } rootUtxo currentTime scope form =
    let
        spentUtxo =
            form.selectedUtxo

        -- Extract signers from the scope owners.
        -- TODO: actually we should let finer grained control from users in the form.
        requiredSigners =
            form.selectedScopeOwners
                |> List.filterMap
                    (\( _, multisig, isChecked ) ->
                        if isChecked then
                            Just <| MultisigScript.extractRequiredSigners multisig

                        else
                            Nothing
                    )
                |> List.concat

        recipients : List TxIntent
        recipients =
            form.recipients
                |> List.filterMap
                    (\{ address, value } ->
                        Address.fromString address
                            |> Maybe.map (\addr -> TxIntent.SendTo addr <| Value.onlyLovelace value)
                    )

        totalValueSpent =
            form.recipients
                |> List.foldl (\{ value } -> N.add value) N.zero
                |> Value.onlyLovelace

        -- Continue building the Tx
        slotConfig =
            case networkId of
                Mainnet ->
                    Uplc.slotConfigMainnet

                Testnet ->
                    Uplc.slotConfigPreview

        slot60sAgo =
            (Time.posixToMillis currentTime - 1000 * 60)
                |> Time.millisToPosix
                |> Uplc.timeToSlot slotConfig

        slotIn6Hours =
            (Time.posixToMillis currentTime + 1000 * 3600 * 6)
                |> Time.millisToPosix
                |> Uplc.timeToSlot slotConfig

        validityRange =
            Just { start = N.toInt slot60sAgo, end = slotIn6Hours }

        -- ( disburseTxIntents, disburseOtherInfo )
        disburseIntentsResult =
            -- TODO: I’m getting the following building error, which makes no sense:
            -- Missing reference script for output reference (ref of output #0 of the 2nd setup Tx, doing stake registrations of each permissions script)
            -- It makes no sense because that output is just the change back to my wallet ...
            disburse networkId rootUtxo scope requiredSigners validityRange spentUtxo (\_ -> recipients) totalValueSpent

        feeSource =
            Cip30.walletChangeAddress wallet
    in
    disburseIntentsResult
        |> Result.andThen
            (\( disburseTxIntents, disburseOtherInfo ) ->
                TxIntent.finalizeAdvanced
                    { govState = TxIntent.emptyGovernanceState
                    , localStateUtxos = localStateUtxos
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource disburseTxIntents
                    , costModels = Uplc.conwayDefaultCostModels
                    }
                    (TxIntent.AutoFee { paymentSource = feeSource })
                    disburseOtherInfo
                    disburseTxIntents
                    |> Result.mapError TxIntent.errorToString
            )


disburse : NetworkId -> OutputReference -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> ( OutputReference, Output ) -> (Value -> List TxIntent) -> Value -> Result String ( List TxIntent, List TxOtherInfo )
disburse networkId rootUtxoRef scope requiredSigners validityRange ( spentUtxoRef, spentOutput ) receivers value =
    let
        ( _, treasuryScript ) =
            scope.sundaeTreasuryScript

        treasuryWitnessSource =
            case scope.sundaeTreasuryScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes treasuryScript

                Just _ ->
                    -- Witness.ByReference ref
                    -- TODO: figure out why Koios is misbehaving and giving incorrect utxos
                    Witness.ByValue <| Script.cborWrappedBytes treasuryScript

        spendConfig : SpendConfig
        spendConfig =
            { scriptWitnessSource = treasuryWitnessSource
            , registryOutputRef = Tuple.first scope.registryUtxo
            , requiredSigners = requiredSigners
            , requiredWithdrawals = requiredWithdrawals
            , spentInputRef = spentUtxoRef
            , spentOutput = spentOutput
            , validityRange = validityRange
            }

        requiredWithdrawals =
            -- Withdrawal with the permissions script
            [ { stakeCredential =
                    { networkId = networkId
                    , stakeCredential = ScriptHash permissionsScriptHash
                    }
              , amount = N.zero
              , scriptWitness =
                    Just <|
                        Witness.Plutus
                            { script =
                                ( Script.plutusVersion permissionsScript
                                , permissionsWitnessSource
                                )
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ( permissionsScriptHash, permissionsScript ) =
            scope.permissionsScript

        permissionsWitnessSource =
            case scope.permissionsScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes permissionsScript

                Just _ ->
                    -- Witness.ByReference ref
                    -- TODO: figure out why Koios is misbehaving and giving incorrect utxos
                    Witness.ByValue <| Script.cborWrappedBytes permissionsScript

        overflowValue =
            Value.subtract value spentOutput.amount
                |> Value.normalize

        ( txIntents, otherIntents ) =
            Treasury.Sundae.disburse spendConfig receivers value
    in
    if overflowValue == Value.zero then
        Ok <| ( txIntents, TxIntent.TxReferenceInput rootUtxoRef :: otherIntents )

    else
        Err <|
            "Trying to disburse more than is available in this UTxO. Overflow value is: "
                ++ String.join "\n" (Value.toMultilineString overflowValue)
                ++ "\n\nSpent output value: "
                ++ String.join "\n" (Value.toMultilineString spentOutput.amount)
                ++ "\n\nDisburse value: "
                ++ String.join "\n" (Value.toMultilineString value)



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | connectedWallet : Maybe Cip30.Wallet
        , msg :
            { from : Msg -> msg
            , cancel : msg
            , buildTx : msg
            }
    }


viewForm : ViewContext a msg -> String -> Form -> Html msg
viewForm { connectedWallet, msg } scopeName form =
    case connectedWallet of
        Nothing ->
            div []
                [ Html.p [] [ text "Please connect your wallet, it will be used to pay the Tx fees." ]
                , Html.button [ onClick msg.cancel ] [ text "Cancel" ]
                ]

        Just _ ->
            div []
                [ viewPickedUtxo form.selectedUtxo
                , Html.map msg.from <| viewSecondSignerPicker scopeName form.selectedScopeOwners
                , Html.map msg.from <| viewRecipientsSection form
                , Html.h4 [] [ text "Tx building" ]
                , Html.button [ onClick msg.buildTx ] [ text "Build Transaction" ]
                , Html.button [ onClick msg.cancel ] [ text "Cancel" ]
                , viewError form.error
                ]


viewPickedUtxo : ( OutputReference, Output ) -> Html msg
viewPickedUtxo utxo =
    Html.p []
        (Html.p [] [ text "Picked UTxO for spending:" ] :: viewDetailedUtxo utxo)


viewSecondSignerPicker : String -> List ( String, MultisigScript, Bool ) -> Html Msg
viewSecondSignerPicker scopeName selectedScopeOwners =
    case scopeName of
        "contingency" ->
            Html.p []
                [ text "All scope owners must approve a disburse action for the contingency scope."
                , Html.ul [] (List.map (\owner -> Html.li [] [ viewScopeOwner owner ]) selectedScopeOwners)
                ]

        _ ->
            Html.p []
                [ Html.p [] [ text "You and at least one other scope owner must approve a disburse action. Pick the signers:" ]
                , div [] (List.map (viewOwnerCheckbox scopeName) selectedScopeOwners)
                ]


viewOwnerCheckbox : String -> ( String, MultisigScript, Bool ) -> Html Msg
viewOwnerCheckbox disbursingScopeName ( name, multisig, checked ) =
    let
        attributes =
            if name == disbursingScopeName then
                -- The disbursing scope must sign, it cannot be unchecked
                [ HA.checked True, HA.disabled True ]

            else
                [ HA.checked checked, HE.onCheck <| CheckOwner name ]
    in
    div []
        [ Html.label []
            [ Html.input (HA.type_ "checkbox" :: attributes) []
            , viewScopeOwner ( name, multisig, () )
            ]
        ]


viewScopeOwner : ( String, MultisigScript, a ) -> Html msg
viewScopeOwner ( name, multisig, _ ) =
    text <| name ++ ": " ++ Debug.toString multisig


viewRecipientsSection : Form -> Html Msg
viewRecipientsSection { recipients } =
    Html.div []
        [ Html.h4 [] [ text "Recipients" ]
        , Html.p []
            [ text "Paste the recipient’s address and value: "
            , Html.button [ onClick AddRecipient ] [ text "add recipient" ]
            ]
        , div [] (List.indexedMap viewRecipient recipients)
        ]


viewRecipient : Int -> { address : String, value : Natural } -> Html Msg
viewRecipient index { address, value } =
    div []
        [ text <| "Recipient " ++ String.fromInt (index + 1) ++ ": "
        , Html.label []
            [ Html.input [ HA.type_ "text", HA.placeholder "paste address", HA.value address, HE.onInput (SetRecipientAddress index) ] [] ]
        , text " "
        , Html.label []
            [ Html.input [ HA.type_ "number", HA.placeholder "value", HA.value (N.toString value), HE.onInput (SetRecipientValue index) ] [] ]
        , text " "
        , Html.button [ onClick (RemoveRecipient index) ] [ text "remove" ]
        ]
