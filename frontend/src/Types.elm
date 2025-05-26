module Types exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Value exposing (Value)
import Integer
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)



-- Registry


type alias ScriptHashRegistry =
    { treasury : Bytes CredentialHash
    , vendor : Bytes CredentialHash
    }


registryTokenName : Bytes AssetName
registryTokenName =
    Bytes.fromText "REGISTRY"



-- Treasury


type alias TreasuryConfiguration =
    { registryToken : Bytes PolicyId
    , permissions : TreasuryPermissions
    , expiration : Natural
    , payoutUpperbound : Natural
    }


treasuryConfigToData : TreasuryConfiguration -> Data
treasuryConfigToData { registryToken, permissions, expiration, payoutUpperbound } =
    Data.Constr N.zero
        [ Data.Bytes <| Bytes.toAny registryToken
        , treasuryPermissionsToData permissions
        , Data.Int <| Integer.fromNatural expiration
        , Data.Int <| Integer.fromNatural payoutUpperbound
        ]


type alias TreasuryPermissions =
    { reorganize : MultisigScript
    , sweep : MultisigScript
    , fund : MultisigScript
    , disburse : MultisigScript
    }


treasuryPermissionsToData : TreasuryPermissions -> Data
treasuryPermissionsToData { reorganize, sweep, fund, disburse } =
    Data.Constr N.zero
        [ MultisigScript.toData reorganize
        , MultisigScript.toData sweep
        , MultisigScript.toData fund
        , MultisigScript.toData disburse
        ]


type TreasurySpendRedeemer
    = Reorganize
    | SweepTreasury
    | Fund Value
    | Disburse Value


treasurySpendRedeemerToData : TreasurySpendRedeemer -> Data
treasurySpendRedeemerToData redeemer =
    case redeemer of
        Reorganize ->
            Data.Constr N.zero []

        SweepTreasury ->
            Data.Constr N.one []

        Fund amount ->
            Data.Constr N.two [ Cardano.Value.toData amount ]

        Disburse amount ->
            Data.Constr N.three [ Cardano.Value.toData amount ]



-- Vendor


type alias VendorConfiguration =
    { registryToken : PolicyId
    , permissions : VendorPermissions
    , expiration : Int
    }


type alias VendorPermissions =
    { pause : MultisigScript
    , resume : MultisigScript
    , modify : MultisigScript
    }


type alias VendorDatum =
    { vendor : MultisigScript
    , payouts : List Payout
    }


type alias Payout =
    { maturation : Int
    , value : Value
    , status : PayoutStatus
    }


type PayoutStatus
    = Active
    | Paused


type VendorSpendRedeemer
    = Withdraw
    | Adjudicate (List PayoutStatus)
    | Modify
    | SweepVendor
    | Malformed
