module Treasury exposing (..)

import Bytes.Comparable exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId, StakeAddress)
import Cardano.Data as Data
import Cardano.MultiAsset as MultiAsset
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxIntent)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Natural exposing (Natural)
import Types exposing (TreasuryConfiguration, TreasurySpendRedeemer(..), treasuryConfigToData, treasurySpendRedeemerToData)


{-| Apply config parameters to the unapplied script to get the final Plutus script.
-}
initializeScript : TreasuryConfiguration -> Script.PlutusScript -> Result String Script.PlutusScript
initializeScript config unappliedScript =
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
    { treasuryScriptBytes : Bytes ScriptCbor
    , requiredSigners : List (Bytes CredentialHash)
    , requiredWithdrawals : List WithdrawalIntent
    , spentInputRef : OutputReference
    , spentOutput : Output
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
sweepBackToCardanoTreasury : SpendConfig -> List TxIntent
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
disburse : SpendConfig -> (Value -> List TxIntent) -> Value -> List TxIntent
disburse config receivers value =
    spend config (Disburse value) receivers value


{-| Reorganize UTxOs of the treasury.
Can be used to merge or split the treasury UTxOs.

The caller is responsible to list the requiredSigners.

-}
reorganize :
    Bytes ScriptCbor
    -> List (Bytes CredentialHash)
    -> List WithdrawalIntent
    -> List ( OutputReference, Output )
    -> (Value -> List TxIntent)
    -> List TxIntent
reorganize treasuryScriptBytes requiredSigners requiredWithdrawals spentUtxos receivers =
    let
        spendings : List TxIntent
        spendings =
            -- Spend all UTxOs, and do not (yet) create the new outputs
            spentUtxos
                |> List.concatMap
                    (\( spentInputRef, spentOutput ) ->
                        spend (SpendConfig treasuryScriptBytes requiredSigners requiredWithdrawals spentInputRef spentOutput)
                            SweepTreasury
                            (\_ -> [])
                            spentOutput.amount
                    )

        totalSpent =
            spentUtxos
                |> List.map (\( _, spentOutput ) -> spentOutput.amount)
                |> Value.sum
    in
    spendings ++ receivers totalSpent


{-| Helper function to spend a UTxO of the treasury
and return the unspent amount into it.
-}
spend : SpendConfig -> TreasurySpendRedeemer -> (Value -> List TxIntent) -> Value -> List TxIntent
spend { treasuryScriptBytes, requiredSigners, requiredWithdrawals, spentInputRef, spentOutput } redeemer receivers value =
    let
        plutusScriptWitness : Witness.PlutusScript
        plutusScriptWitness =
            { script = ( PlutusV3, Witness.ByValue treasuryScriptBytes )
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
    in
    if leftOver == Value.zero then
        spendIntent :: receivers value ++ additionalWithdrawals

    else
        spendIntent :: recreatedOutput :: receivers value ++ additionalWithdrawals
