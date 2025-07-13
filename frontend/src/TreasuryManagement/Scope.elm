module TreasuryManagement.Scope exposing (Scope, setup)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.MultiAsset as MultiAsset
import Cardano.Script exposing (PlutusScript)
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import List.Extra
import MultisigScript exposing (MultisigScript)
import Types


type alias Scope =
    { owner : MultisigScript
    , permissionsScript : ( Bytes CredentialHash, PlutusScript )
    , permissionsScriptRef : Maybe ( OutputReference, Output )
    , sundaeTreasuryScript : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptRef : Maybe ( OutputReference, Output )
    , registryUtxo : ( OutputReference, Output )
    , treasuryUtxos : Utxo.RefDict Output
    }


setup : Bytes TransactionId -> Transaction -> MultisigScript -> Bytes CredentialHash -> ( Bytes CredentialHash, PlutusScript ) -> ( Bytes CredentialHash, PlutusScript ) -> Result String Scope
setup txId registryTx scopeOwner registryScriptHash permissions treasury =
    let
        -- Extract the registry UTxO from the Tx creating all registry UTxOs
        registryUtxoResult : Result String ( OutputReference, Output )
        registryUtxoResult =
            let
                maybeOutputIndex =
                    registryTx.body.outputs
                        |> List.Extra.findIndex (\{ amount } -> MultiAsset.get registryScriptHash Types.registryTokenName amount.assets /= Nothing)
            in
            case maybeOutputIndex of
                Nothing ->
                    Err <| "Registry token " ++ Bytes.toHex registryScriptHash ++ " not found in registry Tx " ++ Bytes.toHex txId

                Just outputIndex ->
                    List.Extra.getAt outputIndex registryTx.body.outputs
                        |> Maybe.map (Tuple.pair (OutputReference txId outputIndex))
                        |> Result.fromMaybe ("Weird, there is not output at index " ++ String.fromInt outputIndex)
    in
    registryUtxoResult
        |> Result.map
            (\utxo ->
                { owner = scopeOwner
                , permissionsScript = permissions
                , permissionsScriptRef = Nothing
                , sundaeTreasuryScript = treasury
                , sundaeTreasuryScriptRef = Nothing
                , registryUtxo = utxo
                , treasuryUtxos = Utxo.emptyRefDict
                }
            )
