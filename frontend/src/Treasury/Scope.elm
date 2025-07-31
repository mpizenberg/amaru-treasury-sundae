module Treasury.Scope exposing (Scope, Scripts, StartDisburseInfo, ViewContext, setup, view, viewDetailedUtxo, viewOwner, viewPermissionsScript, viewRegistryUtxo, viewSetup, viewTreasuryScript)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, NetworkId)
import Cardano.MultiAsset as MultiAsset
import Cardano.Script exposing (PlutusScript)
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra
import MultisigScript exposing (MultisigScript)
import Treasury.SundaeTypes as SundaeTypes


type alias Scope =
    { owner : MultisigScript
    , permissionsScript : ( Bytes CredentialHash, PlutusScript )
    , permissionsScriptRef : Maybe ( OutputReference, Output )
    , sundaeTreasuryScript : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptRef : Maybe ( OutputReference, Output )
    , registryUtxo : ( OutputReference, Output )
    , treasuryUtxos : Utxo.RefDict Output
    }


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , registryTrap : PlutusScript
    , scopesTrap : PlutusScript
    , scopePermissions : PlutusScript
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
                        |> List.Extra.findIndex (\{ amount } -> MultiAsset.get registryScriptHash SundaeTypes.registryTokenName amount.assets /= Nothing)
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



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | networkId : NetworkId
        , rootUtxo : OutputReference
        , allOwners : List ( String, MultisigScript )
        , publishScript : Bytes CredentialHash -> PlutusScript -> msg
        , refreshTreasuryUtxos : msg

        -- TreasuryMergingMsg <| StartMergeUtxos
        , startMergingUtxos : String -> Scope -> OutputReference -> msg
        , startDisburse : StartDisburseInfo -> msg
        , startSwap : StartDisburseInfo -> msg
    }


viewSetup : String -> Scope -> Html msg
viewSetup scopeName { owner, permissionsScript, sundaeTreasuryScript, registryUtxo } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewTreasuryScript sundaeTreasuryScript
        , viewRegistryUtxo registryUtxo
        ]


view : ViewContext a msg -> String -> Scope -> Html msg
view ctx scopeName ({ owner, permissionsScript, permissionsScriptRef, sundaeTreasuryScript, sundaeTreasuryScriptRef, registryUtxo, treasuryUtxos } as scope) =
    let
        treasuryScriptHash =
            Tuple.first sundaeTreasuryScript

        treasuryAddress =
            Address.base ctx.networkId (Address.ScriptHash treasuryScriptHash) (Address.ScriptHash treasuryScriptHash)
    in
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , text <| "Scope address: " ++ Address.toBech32 treasuryAddress
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewPermissionsScriptRef ctx (Tuple.first permissionsScript) (Tuple.second permissionsScript) permissionsScriptRef
        , viewTreasuryScript sundaeTreasuryScript
        , viewTreasuryScriptRef ctx (Tuple.first sundaeTreasuryScript) (Tuple.second sundaeTreasuryScript) sundaeTreasuryScriptRef
        , viewRegistryUtxo registryUtxo
        , viewTreasuryUtxos ctx scopeName scope treasuryUtxos
        ]


viewOwner : MultisigScript -> Html msg
viewOwner owner =
    Html.p [] [ text <| "Owner: " ++ Debug.toString owner ]


viewPermissionsScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewPermissionsScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied permissions script hash: " ++ Bytes.toHex hash ]


viewPermissionsScriptRef : ViewContext a msg -> Bytes CredentialHash -> PlutusScript -> Maybe ( OutputReference, Output ) -> Html msg
viewPermissionsScriptRef ctx hash script maybeUtxo =
    case maybeUtxo of
        Just ( ref, _ ) ->
            Html.p [] [ text <| "Permissions script ref UTxO: " ++ Utxo.refAsString ref ]

        Nothing ->
            Html.p []
                [ text <| "Permissions script ref not published yet. "
                , Html.button [ onClick <| ctx.publishScript hash script ] [ text "Publish script in a ref UTxO" ]
                ]


viewTreasuryScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewTreasuryScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied Sundae treasury script hash: " ++ Bytes.toHex hash ]


viewTreasuryScriptRef : ViewContext a msg -> Bytes CredentialHash -> PlutusScript -> Maybe ( OutputReference, Output ) -> Html msg
viewTreasuryScriptRef ctx hash script maybeUtxo =
    case maybeUtxo of
        Just ( ref, _ ) ->
            Html.p [] [ text <| "Treasury script ref UTxO: " ++ Utxo.refAsString ref ]

        Nothing ->
            Html.p []
                [ text <| "Treasury script ref not published yet. "
                , Html.button [ onClick <| ctx.publishScript hash script ] [ text "Publish script in a ref UTxO" ]
                ]


viewRegistryUtxo : ( OutputReference, Output ) -> Html msg
viewRegistryUtxo ( ref, _ ) =
    Html.p [] [ text <| "Registry UTxO: " ++ Utxo.refAsString ref ]


viewTreasuryUtxos : ViewContext a msg -> String -> Scope -> Utxo.RefDict Output -> Html msg
viewTreasuryUtxos ctx scopeName scope utxos =
    let
        refreshUtxosButton =
            Html.button [ onClick ctx.refreshTreasuryUtxos ] [ text "Refresh UTxOs" ]

        utxosCount =
            Dict.Any.size utxos

        mergeButton =
            if utxosCount > 1 then
                Html.button [ onClick (ctx.startMergingUtxos scopeName scope ctx.rootUtxo) ] [ text "Merge UTxOs" ]

            else
                text ""

        startDisburseInfo : ( OutputReference, Output ) -> StartDisburseInfo
        startDisburseInfo utxo =
            { scopeName = scopeName
            , scope = scope
            , allOwners = ctx.allOwners
            , rootUtxo = ctx.rootUtxo
            , spendingUtxo = utxo
            }

        disburseButton utxo =
            Html.button [ onClick (ctx.startDisburse <| startDisburseInfo utxo) ] [ text "Disburse" ]

        swapButton utxo =
            Html.button [ onClick (ctx.startSwap <| startDisburseInfo utxo) ] [ text "Swap" ]

        viewDetailedUtxoItem utxo =
            Html.li []
                (viewDetailedUtxo utxo ++ [ disburseButton utxo, swapButton utxo ])
    in
    div []
        [ Html.p [] [ text <| "Treasury UTxOs count: " ++ String.fromInt (Dict.Any.size utxos) ]
        , Html.p [] [ text <| "TODO: add buttons for possible actions with those UTxOs" ]
        , refreshUtxosButton
        , mergeButton
        , Html.ul [] <|
            List.map viewDetailedUtxoItem (Dict.Any.toList utxos)
        ]


type alias StartDisburseInfo =
    -- TODO: eventually move into another module?
    { scopeName : String
    , scope : Scope
    , allOwners : List ( String, MultisigScript )
    , rootUtxo : OutputReference
    , spendingUtxo : ( OutputReference, Output )
    }


viewDetailedUtxo : ( OutputReference, Output ) -> List (Html msg)
viewDetailedUtxo ( ref, output ) =
    [ div [] [ text <| "UTxO: " ++ Utxo.refAsString ref ]
    , div [] [ text <| "Address: " ++ Address.toBech32 output.address ]
    , div [] [ text <| "Value: (₳ amounts are in Lovelaces)" ]
    , Html.pre [] [ text <| String.join "\n" <| Value.toMultilineString output.amount ]
    ]
