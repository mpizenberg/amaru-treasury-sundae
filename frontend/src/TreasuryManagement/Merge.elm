module TreasuryManagement.Merge exposing (..)

import Bytes.Comparable exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Script as Script
import Cardano.TxIntent as TxIntent exposing (TxFinalizationError, TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Witness as Witness
import Dict.Any
import Natural as N exposing (Natural)
import Sundae
import Time exposing (Posix)
import TreasuryManagement.Scope exposing (Scope)


type alias BuildContext a =
    { a
        | localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
        , wallet : Cip30.Wallet
    }


buildTx : BuildContext a -> OutputReference -> Posix -> Scope -> List (Bytes CredentialHash) -> Result TxFinalizationError TxFinalized
buildTx { localStateUtxos, networkId, wallet } rootUtxo currentTime scope requiredSigners =
    let
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

        ( mergeTxIntents, mergeOtherInfo ) =
            mergeUtxos networkId rootUtxo scope requiredSigners validityRange

        feeSource =
            Cip30.walletChangeAddress wallet
    in
    TxIntent.finalizeAdvanced
        { govState = TxIntent.emptyGovernanceState
        , localStateUtxos = localStateUtxos
        , coinSelectionAlgo = CoinSelection.largestFirst
        , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource mergeTxIntents
        , costModels = Uplc.conwayDefaultCostModels
        }
        (TxIntent.AutoFee { paymentSource = feeSource })
        mergeOtherInfo
        mergeTxIntents


{-| Merge all UTxOs from a given scope.
-}
mergeUtxos : NetworkId -> OutputReference -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> ( List TxIntent, List TxOtherInfo )
mergeUtxos networkId rootUtxo scope requiredSigners validityRange =
    let
        utxos =
            Dict.Any.toList scope.treasuryUtxos

        permissionsWitnessSource =
            case scope.permissionsScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes permissionsScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        requiredWithdrawals =
            -- Withdrawal with the scope owner script
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
                            , redeemerData = \_ -> Data.Constr N.zero []
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ( permissionsScriptHash, permissionsScript ) =
            scope.permissionsScript

        ( treasuryScriptHash, treasuryScript ) =
            scope.sundaeTreasuryScript

        treasuryWitnessSource =
            case scope.sundaeTreasuryScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes treasuryScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        -- The address must have a stake cred (delegated to always abstain)
        scopeTreasuryAddress =
            Address.base networkId (Address.ScriptHash treasuryScriptHash) (Address.ScriptHash treasuryScriptHash)
    in
    Sundae.reorganize
        { scriptWitnessSource = treasuryWitnessSource
        , registryOutputRef = Tuple.first scope.registryUtxo
        , additionalOutputRefs = [ rootUtxo ]
        , requiredSigners = requiredSigners
        , validityRange = validityRange
        , requiredWithdrawals = requiredWithdrawals
        , spentUtxos = utxos
        , receivers = \value -> [ TxIntent.SendTo scopeTreasuryAddress value ]
        }
