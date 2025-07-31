module SundaeSwap exposing (Order(..), OrderDatum, SingletonValue, orderDatumToData)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, CredentialHash)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName)
import Cardano.Utxo as Utxo exposing (DatumOption)
import Integer as I
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)


type alias OrderDatum =
    { poolId : Maybe (Bytes CredentialHash)
    , owner : MultisigScript
    , scooperFee : Natural
    , destination : { address : Address, datum : Maybe DatumOption }
    , limitOrder : Order
    , extensions : Data
    }


type Order
    = Swap { selling : SingletonValue, buying : SingletonValue }


type alias SingletonValue =
    { policyId : Bytes CredentialHash
    , assetName : Bytes AssetName
    , amount : Natural
    }


{-| Create the Data for an order datum.
-}
orderDatumToData : OrderDatum -> Data
orderDatumToData datum =
    Data.Constr N.zero
        [ Maybe.map (Data.Bytes << Bytes.toAny) datum.poolId
            |> Data.maybe
        , MultisigScript.toData datum.owner
        , Data.Int <| I.fromNatural datum.scooperFee

        -- Todo: actually do the Datum conversion
        , Data.Constr N.zero
            [ Address.toData datum.destination.address
            , Utxo.datumOptionToData datum.destination.datum
            ]
        , orderToData datum.limitOrder
        , datum.extensions
        ]


orderToData : Order -> Data
orderToData order =
    case order of
        Swap { selling, buying } ->
            Data.Constr N.one
                [ Data.List
                    [ Data.Bytes <| Bytes.toAny selling.policyId
                    , Data.Bytes <| Bytes.toAny selling.assetName
                    , Data.Int <| I.fromNatural selling.amount
                    ]
                , Data.List
                    [ Data.Bytes <| Bytes.toAny buying.policyId
                    , Data.Bytes <| Bytes.toAny buying.assetName
                    , Data.Int <| I.fromNatural buying.amount
                    ]
                ]
