module Types exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Value as Value exposing (Value)
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
    , unregister : MultisigScript
    }


treasuryPermissionsToData : TreasuryPermissions -> Data
treasuryPermissionsToData { reorganize, sweep, fund, disburse, unregister } =
    Data.Constr N.zero
        [ MultisigScript.toData reorganize
        , MultisigScript.toData sweep
        , MultisigScript.toData fund
        , MultisigScript.toData disburse
        , MultisigScript.toData unregister
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
            Data.Constr N.two [ Value.toData amount ]

        Disburse amount ->
            Data.Constr N.three [ Value.toData amount ]



-- Vendor


type alias VendorConfiguration =
    { registryToken : Bytes PolicyId
    , permissions : VendorPermissions
    , expiration : Natural
    }


vendorConfigToData : VendorConfiguration -> Data
vendorConfigToData { registryToken, permissions, expiration } =
    Data.Constr N.zero
        [ Data.Bytes <| Bytes.toAny registryToken
        , vendorPermissionsToData permissions
        , Data.Int <| Integer.fromNatural expiration
        ]


type alias VendorPermissions =
    { pause : MultisigScript
    , resume : MultisigScript
    , modify : MultisigScript
    }


vendorPermissionsToData : VendorPermissions -> Data
vendorPermissionsToData { pause, resume, modify } =
    Data.Constr N.zero
        [ MultisigScript.toData pause
        , MultisigScript.toData resume
        , MultisigScript.toData modify
        ]


type alias VendorDatum =
    { vendor : MultisigScript
    , payouts : List Payout
    }


vendorDatumToData : VendorDatum -> Data
vendorDatumToData { vendor, payouts } =
    Data.Constr N.zero
        [ MultisigScript.toData vendor
        , Data.List <| List.map payoutToData payouts
        ]


type alias Payout =
    { maturation : Natural
    , value : Value
    , status : PayoutStatus
    }


payoutToData : Payout -> Data
payoutToData { maturation, value, status } =
    Data.Constr N.zero
        [ Data.Int <| Integer.fromNatural maturation
        , Value.toData value
        , payoutStatusToData status
        ]


type PayoutStatus
    = Active
    | Paused


payoutStatusToData : PayoutStatus -> Data
payoutStatusToData status =
    case status of
        Active ->
            Data.Constr N.zero []

        Paused ->
            Data.Constr N.one []


type VendorSpendRedeemer
    = Withdraw
    | Adjudicate (List PayoutStatus)
    | Modify
    | SweepVendor
    | Malformed


vendorSpendRedeemerToData : VendorSpendRedeemer -> Data
vendorSpendRedeemerToData redeemer =
    case redeemer of
        Withdraw ->
            Data.Constr N.zero []

        Adjudicate statuses ->
            Data.Constr N.one [ Data.List <| List.map payoutStatusToData statuses ]

        Modify ->
            Data.Constr N.two []

        SweepVendor ->
            Data.Constr N.three []

        Malformed ->
            Data.Constr N.four []
