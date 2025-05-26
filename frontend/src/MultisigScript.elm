module MultisigScript exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Integer
import Natural as N exposing (Natural)


{-| Aicone Multisig script type.
Just the script hash.
-}
type MultisigScript
    = Signature (Bytes CredentialHash)
    | AllOf (List MultisigScript)
    | AnyOf (List MultisigScript)
    | AtLeast Int (List MultisigScript)
    | Before Natural
    | After Natural
    | Script (Bytes CredentialHash)


toData : MultisigScript -> Data
toData script =
    case script of
        Signature keyHash ->
            Data.Constr N.zero [ Data.Bytes <| Bytes.toAny keyHash ]

        AllOf multisigs ->
            Data.Constr N.one [ Data.List <| List.map toData multisigs ]

        AnyOf multisigs ->
            Data.Constr N.two [ Data.List <| List.map toData multisigs ]

        AtLeast required multisigs ->
            Data.Constr N.three
                [ Data.Int <| Integer.fromSafeInt required
                , Data.List <| List.map toData multisigs
                ]

        Before time ->
            Data.Constr N.four [ Data.Int <| Integer.fromNatural time ]

        After time ->
            Data.Constr N.five [ Data.Int <| Integer.fromNatural time ]

        Script scriptHash ->
            Data.Constr N.six [ Data.Bytes <| Bytes.toAny scriptHash ]
