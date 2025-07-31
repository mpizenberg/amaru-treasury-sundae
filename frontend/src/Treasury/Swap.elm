module Treasury.Swap exposing (Asset, BuildContext, Form, Msg(..), SwapConfig, ViewContext, buildTx, initForm, update, validateForm, viewForm)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.MultiAsset exposing (AssetName)
import Cardano.TxIntent as TxIntent exposing (TxFinalized, TxIntent(..))
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import SundaeSwap exposing (SingletonValue)
import Time exposing (Posix)
import Treasury.Disburse as Disburse
import Treasury.Scope exposing (Scope, viewDetailedUtxo)
import Utils exposing (viewError)


type alias Form =
    { selectedUtxo : ( OutputReference, Output )
    , selectedScopeOwners : List ( String, MultisigScript, Bool )
    , selling : { asset : Asset, amount : Natural }
    , buying : { asset : Asset, amount : Natural }
    , splitOrder : Int
    , error : Maybe String
    }


type Asset
    = Lovelace
    | Usdm


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
    , selling = { asset = Lovelace, amount = N.zero }
    , buying = { asset = Usdm, amount = N.zero }
    , splitOrder = 1
    , error = Nothing
    }


type alias SwapConfig =
    { selectedUtxo : ( OutputReference, Output )
    , selectedScopeOwners : List MultisigScript
    , poolId : Bytes CredentialHash
    , selling : SingletonValue
    , buying : SingletonValue
    , splitOrder : Int
    }


adaUsdmMainnetPoolId : Bytes CredentialHash
adaUsdmMainnetPoolId =
    Bytes.fromHexUnchecked "64f35d26b237ad58e099041bc14c687ea7fdc58969d7d5b66e2540ef"


usdmMainnetPolicyId : Bytes CredentialHash
usdmMainnetPolicyId =
    Bytes.fromHexUnchecked "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad"


usdmMainnetAssetName : Bytes AssetName
usdmMainnetAssetName =
    Bytes.fromHexUnchecked "0014df105553444d"


adaUsdmTestnetPoolId : Bytes CredentialHash
adaUsdmTestnetPoolId =
    Bytes.fromHexUnchecked "fc55746ffa07021ec0f5b9af7d7bf10bb2e505066f07a84e18f10882"


usdmTestnetPolicyId : Bytes CredentialHash
usdmTestnetPolicyId =
    Bytes.fromHexUnchecked "834a15101873b4e1ddfaa830df46792913995d8738dcde34eda27905"


usdmTestnetAssetName : Bytes AssetName
usdmTestnetAssetName =
    Bytes.fromHexUnchecked "665553444d"


validateForm : NetworkId -> Form -> Result String SwapConfig
validateForm networkId form =
    -- Check that there are at least 2 scope owner selected
    -- Check that the selling and buying assets are a valid pair
    -- Check that the exchange ratio is plausible (0.25 - 10.0?)
    -- Check that the split does not produce orders that are too small.
    -- For example, on the ADA-USDM, there is 2M₳, so it makes little sense
    -- to split the order into chunks lower than 10k₳,
    -- which corresponds to 0.5% slippage.
    let
        selectedScopeOwners =
            List.filter (\( _, _, selected ) -> selected) form.selectedScopeOwners
                |> List.map (\( _, multisig, _ ) -> multisig)

        pool =
            case ( form.selling.asset, form.buying.asset ) of
                ( Lovelace, Usdm ) ->
                    Ok
                        { poolId = poolOf Lovelace Usdm
                        , selling = assetId Lovelace
                        , buying = assetId Usdm
                        }

                ( Usdm, Lovelace ) ->
                    Ok
                        { poolId = poolOf Usdm Lovelace
                        , selling = assetId Usdm
                        , buying = assetId Lovelace
                        }

                _ ->
                    Err <| "Unsupported asset pair: " ++ Debug.toString form.selling.asset ++ " -> " ++ Debug.toString form.buying.asset

        poolOf asset1 asset2 =
            case ( networkId, asset1, asset2 ) of
                ( Mainnet, Lovelace, Usdm ) ->
                    adaUsdmMainnetPoolId

                ( Mainnet, Usdm, Lovelace ) ->
                    adaUsdmMainnetPoolId

                ( Testnet, Lovelace, Usdm ) ->
                    adaUsdmTestnetPoolId

                ( Testnet, Usdm, Lovelace ) ->
                    adaUsdmTestnetPoolId

                _ ->
                    Debug.todo "Should never happen"

        assetId asset =
            case ( networkId, asset ) of
                ( _, Lovelace ) ->
                    ( Bytes.fromHexUnchecked "", Bytes.fromHexUnchecked "" )

                ( Mainnet, Usdm ) ->
                    ( usdmMainnetPolicyId, usdmMainnetAssetName )

                ( Testnet, Usdm ) ->
                    ( usdmTestnetPolicyId, usdmTestnetAssetName )

        swapConfigWithPool : Bytes CredentialHash -> ( Bytes CredentialHash, Bytes AssetName ) -> ( Bytes CredentialHash, Bytes AssetName ) -> SwapConfig
        swapConfigWithPool poolId ( sellingPolicyId, sellingAssetName ) ( buyingPolicyId, buyingAssetName ) =
            { selectedUtxo = form.selectedUtxo
            , selectedScopeOwners = selectedScopeOwners
            , poolId = poolId
            , selling =
                { policyId = sellingPolicyId
                , assetName = sellingAssetName
                , amount = form.selling.amount
                }
            , buying =
                { policyId = buyingPolicyId
                , assetName = buyingAssetName
                , amount = form.buying.amount
                }
            , splitOrder = form.splitOrder
            }

        -- Check that the exchange ratio is plausible (0.25 - 10.0?)
        plausibleExchangeRate =
            let
                nToFloat n =
                    n |> N.toString |> String.toFloat |> Maybe.withDefault 0

                rate =
                    nToFloat form.buying.amount
                        / nToFloat form.selling.amount
            in
            case ( form.selling.asset, form.buying.asset ) of
                ( Lovelace, Usdm ) ->
                    -- 1 ada in [$0.25-$10] range
                    if rate >= 0.25 && rate <= 10.0 then
                        Ok ()

                    else
                        Err "ADA->USDM rate out of the (0.25-10.0) safe range."

                ( Usdm, Lovelace ) ->
                    -- 1 ada in [$0.10-$2] range
                    if rate >= 0.5 && rate <= 10.0 then
                        Ok ()

                    else
                        Err "USDM->ADA rate out of the (0.5-10.0) safe range (1 ada in [$0.10-$2])."

                -- No check on pairs other than ADA-USDM
                _ ->
                    Ok ()

        plausibleAmount { asset, amount } =
            case asset of
                Lovelace ->
                    if amount |> N.isGreaterThanOrEqual (N.fromSafeInt 1000000) then
                        Ok ()

                    else
                        Err "Lovelace amount too low, maybe you forgot to add the 6 decimals?"

                Usdm ->
                    if amount |> N.isGreaterThanOrEqual (N.fromSafeInt 1000000) then
                        Ok ()

                    else
                        Err "USDM amount too low, maybe you forgot to add the 6 decimals?"
    in
    if List.length selectedScopeOwners < 2 then
        Err "At least 2 scope owners must be selected"

    else
        let
            -- Check that the split does not produce orders that are too small.
            -- For example, on the ADA-USDM, there is 2M₳, so it makes little sense
            -- to split the order into chunks lower than 10k₳,
            -- which corresponds to 0.5% slippage.
            checkSplitSizeIsPlausible swap =
                let
                    splitAmount =
                        swap.selling.amount
                            |> N.divBy (N.fromSafeInt swap.splitOrder)
                            |> Maybe.withDefault N.zero
                in
                -- ADA-USDM on Mainnet
                if swap.poolId == adaUsdmMainnetPoolId then
                    -- Ada amount suggested 5k <= ?₳ <= 20k
                    if swap.selling.policyId == Bytes.empty then
                        if (splitAmount |> N.isLessThan (N.fromSafeInt 5000000000)) && (swap.splitOrder > 1) then
                            Err "Please lower the split count to get orders >= 5k₳. The ADA-USDM pool is roughly 2M₳ so a 5k₳ order is roughly 0.25% slippage. There is no point splitting it more."

                        else if splitAmount |> N.isGreaterThan (N.fromSafeInt 20000000000) then
                            Err "Please increase the split count to get orders <= 20k₳. The ADA-USDM pool is roughly 2M₳ so a 20k₳ order is roughly 1% slippage. Any more slippage is waste territory."

                        else
                            Ok swap

                    else if (splitAmount |> N.isLessThan (N.fromSafeInt 5000000000)) && (swap.splitOrder > 1) then
                        -- In the case of USDM, use the same range 5k <= ?$ <= 20k
                        Err "Please lower the split count to get orders >= $5k. The ADA-USDM pool is roughly $2M so a $5k order is roughly 0.25% slippage. There is no point splitting it more."

                    else if splitAmount |> N.isGreaterThan (N.fromSafeInt 20000000000) then
                        Err "Please increase the split count to get orders <= $20k. The ADA-USDM pool is roughly $2M so a $20k order is roughly 1% slippage. Any more slippage is waste territory."

                    else
                        Ok swap

                else if swap.poolId == adaUsdmTestnetPoolId then
                    -- ADA-USDM on Testnet
                    -- Ada amount suggested 5 <= ?₳ <= 20
                    if swap.selling.policyId == Bytes.empty then
                        if (splitAmount |> N.isLessThan (N.fromSafeInt 5000000)) && (swap.splitOrder > 1) then
                            Err "Please lower the split count to get orders >= 5₳. The ADA-USDM Preview pool is roughly 2k₳ so a 5₳ order is roughly 0.25% slippage. There is no point splitting it more."

                        else if splitAmount |> N.isGreaterThan (N.fromSafeInt 20000000) then
                            Err "Please increase the split count to get orders <= 20₳. The ADA-USDM Preview pool is roughly 2k₳ so a 20₳ order is roughly 1% slippage. Any more slippage is waste territory."

                        else
                            Ok swap

                    else if (splitAmount |> N.isLessThan (N.fromSafeInt 5000000)) && (swap.splitOrder > 1) then
                        -- In the case of USDM, use the same range 5 <= ?$ <= 20
                        Err "Please lower the split count to get orders >= $5. The ADA-USDM Preview pool is roughly $2k so a $5 order is roughly 0.25% slippage. There is no point splitting it more."

                    else if splitAmount |> N.isGreaterThan (N.fromSafeInt 20000000) then
                        Err "Please increase the split count to get orders <= $20. The ADA-USDM Preview pool is roughly $2k so a $20 order is roughly 1% slippage. Any more slippage is waste territory."

                    else
                        Ok swap

                else
                    Ok swap
        in
        Result.map4 (\{ poolId, selling, buying } _ _ _ -> swapConfigWithPool poolId selling buying)
            pool
            (plausibleAmount form.selling)
            (plausibleAmount form.buying)
            plausibleExchangeRate
            |> Result.andThen checkSplitSizeIsPlausible



-- UPDATE ############################################################


type Msg
    = CheckOwner String Bool
    | PickSellingAsset Asset
    | PickBuyingAsset Asset
    | SetSellingAmount Natural
    | SetBuyingAmount Natural
    | SplitOrder Int


update : Msg -> Form -> Form
update msg ({ selectedScopeOwners, selling, buying } as form) =
    case msg of
        CheckOwner scopeOwnerName isChecked ->
            let
                updatedSelection =
                    List.Extra.updateIf (\( name, _, _ ) -> name == scopeOwnerName)
                        (\( name, multisig, _ ) -> ( name, multisig, isChecked ))
                        selectedScopeOwners
            in
            { form | selectedScopeOwners = updatedSelection }

        PickSellingAsset asset ->
            { form | selling = { selling | asset = asset } }

        PickBuyingAsset asset ->
            { form | buying = { buying | asset = asset } }

        SetSellingAmount amount ->
            { form | selling = { selling | amount = amount } }

        SetBuyingAmount amount ->
            { form | buying = { buying | amount = amount } }

        SplitOrder order ->
            { form | splitOrder = order }


type alias BuildContext a =
    { a
        | localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
        , wallet : Cip30.Wallet
    }



-- Mainnet split order Tx: https://beta.cexplorer.io/tx/3cf9f4f352cf918f1ae7255a8fbb3b61c7d68236e4f56d810d9dad7f11f211f4?tab=content
-- Mainnet config:
--   - Pool ident Ada-USDM: 64f35d26b237ad58e099041bc14c687ea7fdc58969d7d5b66e2540ef
--   - USDM policy ID: c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad
--   - USDM raw asset name (333) USDM: 0014df105553444d
-- Preview order Tx: https://preview.cardanoscan.io/transaction/afe6b10b51811ec240a2c268662e6374ede36a8bba6f73baba68fb5405574582?tab=utxo
-- Preview config:
--   - Pool ident Ada-fUSDM: fc55746ffa07021ec0f5b9af7d7bf10bb2e505066f07a84e18f10882
--   - fUSDM policy ID: 834a15101873b4e1ddfaa830df46792913995d8738dcde34eda27905
--   - fUSDM raw asset name: 665553444d


sundaeOrderContractHashMainnet : Bytes CredentialHash
sundaeOrderContractHashMainnet =
    Bytes.fromHexUnchecked "fa6a58bbe2d0ff05534431c8e2f0ef2cbdc1602a8456e4b13c8f3077"


sundaeOrderContractHashPreview : Bytes CredentialHash
sundaeOrderContractHashPreview =
    Bytes.fromHexUnchecked "cfad1914b599d18bffd14d2bbd696019c2899cbdd6a03325cdf680bc"


defaultScooperFee : NetworkId -> Natural
defaultScooperFee networkId =
    -- Scooper fee is determined by a "setting" UTxO,
    -- which can be found on any scoop Tx.
    -- Currently, the total of the different fees is:
    --   - 0.5 ada on Preview
    --   - 1.28 ada on Mainnet
    case networkId of
        Mainnet ->
            N.fromSafeInt 1280000

        Testnet ->
            N.fromSafeInt 500000


buildTx : BuildContext a -> OutputReference -> Posix -> Scope -> SwapConfig -> Result String TxFinalized
buildTx { localStateUtxos, networkId, wallet } rootUtxo currentTime scope swap =
    let
        (( _, spentOutput ) as spentUtxo) =
            swap.selectedUtxo

        -- Extract signers from the scope owners.
        -- TODO: actually we should let finer grained control from users in the form.
        requiredSigners =
            swap.selectedScopeOwners
                |> List.concatMap MultisigScript.extractRequiredSigners

        -- Compute the selling amounts for each of the split order
        ( sellingQuotient, sellingRemainder ) =
            swap.selling.amount
                |> N.divModBy (N.fromSafeInt swap.splitOrder)
                |> Maybe.withDefault ( N.zero, N.zero )

        sellingAmounts =
            N.add sellingQuotient sellingRemainder
                :: List.repeat (swap.splitOrder - 1) sellingQuotient

        -- Now do the same for the minimum buying amounts
        ( buyingQuotient, buyingRemainder ) =
            swap.buying.amount
                |> N.divModBy (N.fromSafeInt swap.splitOrder)
                |> Maybe.withDefault ( N.zero, N.zero )

        buyingAmounts =
            N.add buyingQuotient buyingRemainder
                :: List.repeat (swap.splitOrder - 1) buyingQuotient

        -- Recipients are each order of the split
        recipients : List TxIntent
        recipients =
            List.map2 createSwapOrder sellingAmounts buyingAmounts

        createSwapOrder : Natural -> Natural -> TxIntent
        createSwapOrder sellingAmount buyingAmount =
            let
                orderContractAddress =
                    case networkId of
                        Mainnet ->
                            Address.script networkId sundaeOrderContractHashMainnet

                        Testnet ->
                            Address.script networkId sundaeOrderContractHashPreview

                -- 2 ada deposit
                depositAmount =
                    N.fromSafeInt 2000000

                feePlusDeposit =
                    Value.onlyLovelace <| N.add depositAmount <| defaultScooperFee networkId

                orderTotalValue =
                    if Bytes.isEmpty swap.selling.policyId then
                        Value.onlyLovelace sellingAmount
                            |> Value.add feePlusDeposit

                    else
                        Value.onlyToken swap.selling.policyId swap.selling.assetName sellingAmount
                            |> Value.add feePlusDeposit
            in
            TxIntent.SendToOutput
                { address =
                    orderContractAddress
                        |> Address.setShelleyStakeCred (Address.extractStakeCredential spentOutput.address)
                , amount = orderTotalValue
                , datumOption =
                    Just <|
                        Utxo.datumValueFromData <|
                            SundaeSwap.orderDatumToData <|
                                { poolId = Just swap.poolId
                                , owner = scope.owner
                                , scooperFee = defaultScooperFee networkId
                                , destination = { address = spentOutput.address, datum = Nothing }
                                , limitOrder =
                                    SundaeSwap.Swap
                                        { selling =
                                            { policyId = swap.selling.policyId
                                            , assetName = swap.selling.assetName
                                            , amount = sellingAmount
                                            }
                                        , buying =
                                            { policyId = swap.buying.policyId
                                            , assetName = swap.buying.assetName
                                            , amount = buyingAmount
                                            }
                                        }
                                , extensions = Data.List []
                                }
                , referenceScript = Nothing
                }

        totalValueSpent =
            recipients
                |> List.foldl (\intent -> Value.add <| extractOutput intent) Value.zero

        extractOutput intent =
            case intent of
                SendToOutput { amount } ->
                    amount

                _ ->
                    Value.zero

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
            -- SOLUTION: it’s a problem with Koios ref scripts: https://github.com/cardano-community/koios-artifacts/issues/372
            Disburse.disburse networkId rootUtxo scope requiredSigners validityRange spentUtxo (\_ -> recipients) totalValueSpent

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



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | connectedWallet : Maybe Cip30.Wallet
        , networkId : NetworkId
        , msg :
            { from : Msg -> msg
            , cancel : msg
            , buildTx : msg
            }
    }


viewForm : ViewContext a msg -> String -> Form -> Html msg
viewForm { connectedWallet, networkId, msg } scopeName form =
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
                , Html.map msg.from <| viewSwapSection networkId form
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
                [ text "All scope owners must approve a swap action for the contingency scope."
                , Html.ul [] (List.map (\owner -> Html.li [] [ viewScopeOwner owner ]) selectedScopeOwners)
                ]

        _ ->
            Html.p []
                [ Html.p [] [ text "You and at least one other scope owner must approve a swap action. Pick the signers:" ]
                , div [] (List.map (viewOwnerCheckbox scopeName) selectedScopeOwners)
                ]


viewOwnerCheckbox : String -> ( String, MultisigScript, Bool ) -> Html Msg
viewOwnerCheckbox swapScopeName ( name, multisig, checked ) =
    let
        attributes =
            if name == swapScopeName then
                -- The swapping scope must sign, it cannot be unchecked
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


viewSwapSection : NetworkId -> Form -> Html Msg
viewSwapSection networkId { selling, buying, splitOrder } =
    let
        selectedStyle asset1 asset2 =
            if asset1 == asset2 then
                [ HA.style "font-weight" "bold", HA.style "text-decoration" "underline" ]

            else
                []

        assetName asset =
            case asset of
                Lovelace ->
                    "ADA"

                Usdm ->
                    "USDM"

        decimals asset =
            case asset of
                Lovelace ->
                    6

                Usdm ->
                    6

        prettyAmount asset amount =
            let
                divisor =
                    ("1" ++ String.repeat (decimals asset) "0")
                        |> N.fromSafeString

                amountAsString =
                    case amount |> N.divModBy divisor of
                        Just ( quotient, remainder ) ->
                            N.toString quotient ++ "." ++ String.padLeft (decimals asset) '0' (N.toString remainder)

                        Nothing ->
                            "0"
            in
            amountAsString ++ " " ++ assetName asset

        nToFloat n =
            n |> N.toString |> String.toFloat |> Maybe.withDefault 0

        swapPriceBuyPerSell =
            nToFloat buying.amount / nToFloat selling.amount

        swapPriceSellPerBuy =
            1.0 / swapPriceBuyPerSell

        chunkSize =
            selling.amount
                |> N.divBy (N.fromSafeInt splitOrder)
                |> Maybe.withDefault N.zero

        dexLink =
            case networkId of
                Testnet ->
                    let
                        assetUrlArg asset =
                            case asset of
                                Lovelace ->
                                    "ada.lovelace"

                                Usdm ->
                                    Bytes.toHex usdmTestnetPolicyId ++ "." ++ Bytes.toHex usdmTestnetAssetName
                    in
                    "https://app.preview.sundae.fi/exchange?given="
                        ++ assetUrlArg selling.asset
                        ++ "&taken="
                        ++ assetUrlArg buying.asset

                Mainnet ->
                    Debug.todo ""
    in
    Html.div []
        [ Html.h4 [] [ text "Swap" ]
        , Html.p []
            [ text "Selling asset: "
            , Html.button ((onClick <| PickSellingAsset Lovelace) :: selectedStyle selling.asset Lovelace) [ text <| assetName Lovelace ]
            , Html.button ((onClick <| PickSellingAsset Usdm) :: selectedStyle selling.asset Usdm) [ text <| assetName Usdm ]
            ]
        , Html.label []
            [ text "Selling amount (with decimals): "
            , Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.placeholder "1000000"
                , HA.value <| N.toString selling.amount
                , HE.onInput <| \str -> SetSellingAmount (Maybe.withDefault selling.amount <| N.fromDecimalString str)
                ]
                []
            , text <| " " ++ prettyAmount selling.asset selling.amount
            ]
        , Html.p []
            [ text "Buying asset: "
            , Html.button ((onClick <| PickBuyingAsset Lovelace) :: selectedStyle buying.asset Lovelace) [ text <| assetName Lovelace ]
            , Html.button ((onClick <| PickBuyingAsset Usdm) :: selectedStyle buying.asset Usdm) [ text <| assetName Usdm ]
            ]
        , Html.label []
            [ text "Buying amount (with decimals): "
            , Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.placeholder "1000000"
                , HA.value <| N.toString buying.amount
                , HE.onInput <| \str -> SetBuyingAmount (Maybe.withDefault buying.amount <| N.fromDecimalString str)
                ]
                []
            , text <| " " ++ prettyAmount buying.asset buying.amount
            ]
        , Html.p []
            [ text "Swap price: "
            , text <| String.fromFloat swapPriceBuyPerSell ++ " " ++ assetName buying.asset ++ " / " ++ assetName selling.asset
            , text " --- "
            , text <| String.fromFloat swapPriceSellPerBuy ++ " " ++ assetName selling.asset ++ " / " ++ assetName buying.asset
            ]
        , Html.p []
            [ text "DEX link for reference help: "
            , Html.a
                [ HA.href dexLink
                , HA.target "_blank"
                ]
                [ text <| "View pair in aggregator ↗" ]
            ]
        , Html.p [] <|
            [ Html.label []
                [ text "Split order: "
                , Html.input
                    [ HA.type_ "number"
                    , HA.min "1"
                    , HA.placeholder "1"
                    , HA.style "width" "4em"
                    , HA.value (String.fromInt splitOrder)
                    , HE.onInput <| \str -> SplitOrder (Maybe.withDefault splitOrder <| String.toInt str)
                    ]
                    []
                ]
            , text " --- chunk size: "
            , text <| prettyAmount selling.asset chunkSize
            ]
        , Html.p [] [ text <| "Remark that each split will add a 2₳ deposit (returned on completion) and some batcher fees." ]
        ]
