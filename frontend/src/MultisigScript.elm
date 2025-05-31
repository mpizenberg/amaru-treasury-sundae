module MultisigScript exposing (..)

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Integer as I
import Maybe.Extra
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


fromData : Data -> Maybe MultisigScript
fromData data =
    let
        toCred : Bytes Any -> Bytes CredentialHash
        toCred =
            Bytes.toHex >> Bytes.fromHexUnchecked
    in
    case data of
        Data.Constr variant [ Data.Bytes hash ] ->
            if variant == N.zero then
                Just <| Signature <| toCred hash

            else if variant == N.six then
                Just <| Script <| toCred hash

            else
                Nothing

        Data.Constr variant [ Data.Int time ] ->
            if variant == N.four then
                Just <| Before <| I.toNatural time

            else if variant == N.five then
                Just <| After <| I.toNatural time

            else
                Nothing

        Data.Constr variant [ Data.List multisigsData ] ->
            if variant == N.one then
                Maybe.Extra.combineMap fromData multisigsData
                    |> Maybe.map AllOf

            else if variant == N.two then
                Maybe.Extra.combineMap fromData multisigsData
                    |> Maybe.map AnyOf

            else
                Nothing

        Data.Constr variant [ Data.Int required, Data.List multisigsData ] ->
            if variant == N.three then
                Maybe.Extra.combineMap fromData multisigsData
                    |> Maybe.map (AtLeast <| I.toInt required)

            else
                Nothing

        _ ->
            Nothing


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
                [ Data.Int <| I.fromSafeInt required
                , Data.List <| List.map toData multisigs
                ]

        Before time ->
            Data.Constr N.four [ Data.Int <| I.fromNatural time ]

        After time ->
            Data.Constr N.five [ Data.Int <| I.fromNatural time ]

        Script scriptHash ->
            Data.Constr N.six [ Data.Bytes <| Bytes.toAny scriptHash ]
