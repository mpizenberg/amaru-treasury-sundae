module Treasury.Sundae exposing (..)

import Bytes.Comparable exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId, StakeAddress)
import Cardano.Data as Data
import Cardano.MultiAsset as MultiAsset
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Cbor.Encode as CE
import Natural exposing (Natural)
import Treasury.SundaeTypes as SundaeTypes exposing (TreasuryConfiguration, TreasurySpendRedeemer(..), treasuryConfigToData, treasurySpendRedeemerToData)


{-| Apply config parameters to the unapplied script to get the final Plutus script.
-}
initializeScript : TreasuryConfiguration -> Script.PlutusScript -> Result String Script.PlutusScript
initializeScript config unappliedScript =
    let
        _ =
            Data.toCborUplc (treasuryConfigToData config)
                |> CE.encode
                |> Bytes.Comparable.fromBytes
                |> Bytes.Comparable.toHex
                |> Debug.log "treasury config"
    in
    Uplc.applyParamsToScript [ treasuryConfigToData config ] unappliedScript


{-| Withraw Ada from the treasury reward account.
-}
initialWithdrawal : NetworkId -> Bytes CredentialHash -> Bytes ScriptCbor -> Natural -> List TxIntent
initialWithdrawal networkId treasuryScriptHash treasuryScriptBytes amount =
    let
        treasuryRewardAccount : StakeAddress
        treasuryRewardAccount =
            { networkId = networkId
            , stakeCredential = ScriptHash treasuryScriptHash
            }

        plutusScriptWitness : Witness.PlutusScript
        plutusScriptWitness =
            { script = ( PlutusV3, Witness.ByValue treasuryScriptBytes )
            , redeemerData = \_ -> Data.List []
            , requiredSigners = []
            }
    in
    [ TxIntent.WithdrawRewards
        { stakeCredential = treasuryRewardAccount
        , amount = amount
        , scriptWitness = Just <| Witness.Plutus plutusScriptWitness
        }
    , TxIntent.SendTo (Address.script networkId treasuryScriptHash)
        (Value.onlyLovelace amount)
    ]


type alias SpendConfig =
    { scriptWitnessSource : Witness.Source (Bytes ScriptCbor)
    , registryOutputRef : OutputReference
    , requiredSigners : List (Bytes CredentialHash)
    , requiredWithdrawals : List WithdrawalIntent
    , spentInputRef : OutputReference
    , spentOutput : Output
    , validityRange : Maybe { start : Int, end : Natural }
    }


type alias WithdrawalIntent =
    { stakeCredential : StakeAddress
    , amount : Natural
    , scriptWitness : Maybe Witness.Script
    }


{-| Sweep funds back to the Cardano Treasury.

The caller is responsible to list the requiredSigners
and to provide required additional withdrawals.

WARNING: There cannot be 2 withdrawals for the same credential,
so if spending multiple utxos, make sure you only add the withdrawals once.

If the contract has expired, there is no required signer.
If not, the caller decides which of the signers are required
to satisfy the sweep multisig in the treasury configuration.

-}
sweepBackToCardanoTreasury : SpendConfig -> ( List TxIntent, List TxOtherInfo )
sweepBackToCardanoTreasury ({ spentOutput } as config) =
    let
        donation : Value -> List TxIntent
        donation value =
            [ TxIntent.SendTo (Debug.todo "Treasury Donation!!!") value ]

        isOnlyAda =
            MultiAsset.isEmpty spentOutput.amount.assets

        donationAmount =
            if isOnlyAda then
                spentOutput.amount.lovelace

            else
                Utxo.freeAda spentOutput
    in
    spend config SweepTreasury donation (Value.onlyLovelace donationAmount)


{-| Disburse funds to some destination.

The caller is responsible to list the requiredSigners
and to provide required additional withdrawals.

WARNING: There cannot be 2 withdrawals for the same credential,
so if spending multiple utxos, make sure you only add the withdrawals once.

-}
disburse : SpendConfig -> (Value -> List TxIntent) -> Value -> ( List TxIntent, List TxOtherInfo )
disburse config receivers value =
    spend config (Disburse value) receivers value


{-| Config for reorganizing the treasury UTxOs.
-}
type alias ReorganizeConfig =
    { scriptWitnessSource : Witness.Source (Bytes ScriptCbor)
    , registryOutputRef : OutputReference
    , additionalOutputRefs : List OutputReference
    , requiredSigners : List (Bytes CredentialHash)
    , validityRange : Maybe { start : Int, end : Natural }
    , requiredWithdrawals : List WithdrawalIntent
    , spentUtxos : List ( OutputReference, Output )
    , receivers : Value -> List TxIntent
    }


{-| Reorganize UTxOs of the treasury.
Can be used to merge or split the treasury UTxOs.

The caller is responsible to list the requiredSigners, and the validity range.

-}
reorganize : ReorganizeConfig -> ( List TxIntent, List TxOtherInfo )
reorganize { scriptWitnessSource, registryOutputRef, additionalOutputRefs, requiredSigners, validityRange, requiredWithdrawals, spentUtxos, receivers } =
    let
        -- (List TxIntent, List TxOtherInfo)
        ( spendIntents, spendOthers ) =
            -- Spend all UTxOs, and do not (yet) create the new outputs
            spentUtxos
                |> List.map
                    (\( spentInputRef, spentOutput ) ->
                        spend (SpendConfig scriptWitnessSource registryOutputRef requiredSigners requiredWithdrawals spentInputRef spentOutput validityRange)
                            SundaeTypes.Reorganize
                            (\_ -> [])
                            spentOutput.amount
                    )
                |> List.foldr (\( intents, others ) ( accIntents, accOthers ) -> ( intents ++ accIntents, others ++ accOthers )) ( [], [] )

        totalSpent =
            spentUtxos
                |> List.map (\( _, spentOutput ) -> spentOutput.amount)
                |> Value.sum
    in
    ( spendIntents ++ receivers totalSpent
    , spendOthers ++ List.map TxIntent.TxReferenceInput additionalOutputRefs
    )


{-| Helper function to spend a UTxO of the treasury
and return the unspent amount into it.
-}
spend : SpendConfig -> TreasurySpendRedeemer -> (Value -> List TxIntent) -> Value -> ( List TxIntent, List TxOtherInfo )
spend { scriptWitnessSource, registryOutputRef, requiredSigners, requiredWithdrawals, spentInputRef, spentOutput, validityRange } redeemer receivers value =
    let
        plutusScriptWitness : Witness.PlutusScript
        plutusScriptWitness =
            { script = ( PlutusV3, scriptWitnessSource )
            , redeemerData = \_ -> treasurySpendRedeemerToData redeemer
            , requiredSigners = requiredSigners
            }

        leftOver =
            Value.subtract spentOutput.amount value
                |> Value.normalize

        recreatedOutput =
            TxIntent.SendToOutput { spentOutput | amount = leftOver }

        spendIntent =
            TxIntent.Spend <|
                FromPlutusScript
                    { spentInput = spentInputRef
                    , datumWitness = Nothing
                    , plutusScriptWitness = plutusScriptWitness
                    }

        additionalWithdrawals =
            List.map TxIntent.WithdrawRewards requiredWithdrawals

        otherInfoIntent =
            case validityRange of
                Nothing ->
                    [ TxIntent.TxReferenceInput registryOutputRef ]

                Just range ->
                    [ TxIntent.TxReferenceInput registryOutputRef
                    , TxIntent.TxTimeValidityRange range
                    ]
    in
    if leftOver == Value.zero then
        ( spendIntent :: receivers value ++ additionalWithdrawals
        , otherInfoIntent
        )

    else
        ( spendIntent :: recreatedOutput :: receivers value ++ additionalWithdrawals
        , otherInfoIntent
        )
