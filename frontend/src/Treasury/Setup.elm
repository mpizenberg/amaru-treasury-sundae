module Treasury.Setup exposing (Form, LastStepParams, Msg(..), SetupTxs, Validation, ViewContext, amaruTreasury, initForm, updateForm, viewForm)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address, CredentialHash, NetworkId)
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Script as Script exposing (PlutusScript)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (TxFinalizationError, TxFinalized)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Cardano.Witness as Witness
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import Integer as I
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Page.SignTx as SignTx
import Time exposing (Posix)
import Treasury.Loading exposing (Loaded)
import Treasury.LoadingParams exposing (LoadingParams)
import Treasury.Scope as Scope exposing (Scope, Scripts)
import Treasury.Scopes as Scopes exposing (Scopes)
import Treasury.Sundae
import Treasury.SundaeTypes as SundaeTypes
import Utils exposing (displayPosixDate, viewError)


type alias Form =
    { ledgerOwner : String
    , consensusOwner : String
    , mercenariesOwner : String
    , marketingOwner : String
    , expiration : Int
    , validation : Maybe Validation
    }


type alias Validation =
    Result String { expiration : Int, scopeOwners : Scopes MultisigScript }


initForm : Posix -> Form
initForm currentTime =
    { ledgerOwner = ""
    , consensusOwner = ""
    , mercenariesOwner = ""
    , marketingOwner = ""
    , expiration = Time.posixToMillis currentTime
    , validation = Nothing
    }



-- Update


type Msg
    = LedgerOwnerField String
    | ConsensusOwnerField String
    | MercenariesOwnerField String
    | MarketingOwnerField String
    | ExpirationField String
    | ValidateSetupForm


updateForm : Msg -> Form -> Form
updateForm msg form =
    case msg of
        LedgerOwnerField value ->
            { form | ledgerOwner = value }

        ConsensusOwnerField value ->
            { form | consensusOwner = value }

        MercenariesOwnerField value ->
            { form | mercenariesOwner = value }

        MarketingOwnerField value ->
            { form | marketingOwner = value }

        ExpirationField value ->
            { form | expiration = Maybe.withDefault 0 <| String.toInt value }

        ValidateSetupForm ->
            { form | validation = Just <| validate form }


validate : Form -> Validation
validate form =
    Scopes form.ledgerOwner form.consensusOwner form.mercenariesOwner form.marketingOwner
        |> Scopes.map validateKeyHash
        |> Scopes.toResult
        -- TODO: add some validation of the expiration Int
        |> Result.map (\scopes -> { expiration = form.expiration, scopeOwners = scopes })


validateKeyHash : String -> Result String MultisigScript
validateKeyHash hex =
    case Bytes.fromHex hex of
        Nothing ->
            Err <| "Invalid hex string: " ++ hex

        Just bytes ->
            if Bytes.width bytes /= 28 then
                Err <| "This credential is not 28 bytes long: " ++ Bytes.toHex bytes

            else
                Ok (MultisigScript.Signature bytes)



-- Setup


type alias LastStepParams =
    { txs :
        { scopes : { txId : Bytes TransactionId, finalized : TxFinalized }
        , permissions : { txId : Bytes TransactionId, finalized : TxFinalized }
        }
    , rootUtxo : ( OutputReference, Output )
    , scopeOwners : Scopes MultisigScript
    , scopesPermissions : Scopes ( Bytes CredentialHash, PlutusScript )
    , scopesRegistries : Scopes ( Bytes CredentialHash, PlutusScript )
    , contingencyPermissions : ( Bytes CredentialHash, PlutusScript )
    , contingencyRegistry : ( Bytes CredentialHash, PlutusScript )
    , loadingParams : LoadingParams
    }


type alias SetupTxs =
    { scopes : SignTx.Prep
    , permissions : SignTx.Prep
    , registries : SignTx.Prep
    }


amaruTreasury : Utxo.RefDict Output -> Scripts -> NetworkId -> Cip30.Wallet -> { expiration : Int, scopeOwners : Scopes MultisigScript } -> Result String ( SetupTxs, Loaded )
amaruTreasury localStateUtxos scripts networkId connectedWallet { expiration, scopeOwners } =
    setupAmaruScopes localStateUtxos scripts.scopesTrap networkId connectedWallet scopeOwners
        |> Result.andThen
            (\( ( scopesSeedUtxo, setupScopesTx ), ( scopesTrapScriptHash, _ ) ) ->
                let
                    scopesTxId =
                        Transaction.computeTxId setupScopesTx.tx

                    localStateAfterAmaruScopes =
                        (TxIntent.updateLocalState scopesTxId setupScopesTx.tx localStateUtxos).updatedState
                in
                setupPermissions localStateAfterAmaruScopes scripts.scopePermissions connectedWallet scopesTrapScriptHash
                    |> Result.andThen
                        (\( setupPermissionsTx, scopesPermissions, contingencyPermissions ) ->
                            let
                                permissionsTxId =
                                    Transaction.computeTxId setupPermissionsTx.tx

                                localStateAfterPermissions =
                                    (TxIntent.updateLocalState permissionsTxId setupPermissionsTx.tx localStateAfterAmaruScopes).updatedState
                            in
                            pickSeedUtxo localStateAfterPermissions (Cip30.walletChangeAddress connectedWallet) (N.fromSafeInt 10000000)
                                |> Result.andThen
                                    (\( registriesSeedRef, _ ) ->
                                        setupRegistries scripts.registryTrap registriesSeedRef
                                            |> Result.andThen
                                                (\( scopesRegistries, contingencyRegistry ) ->
                                                    setupLastStep localStateAfterPermissions
                                                        scripts
                                                        networkId
                                                        connectedWallet
                                                        { txs =
                                                            { scopes = { txId = scopesTxId, finalized = setupScopesTx }
                                                            , permissions = { txId = permissionsTxId, finalized = setupPermissionsTx }
                                                            }
                                                        , rootUtxo = scopesSeedUtxo
                                                        , scopeOwners = scopeOwners
                                                        , scopesPermissions = scopesPermissions
                                                        , scopesRegistries = scopesRegistries
                                                        , contingencyPermissions = contingencyPermissions
                                                        , contingencyRegistry = contingencyRegistry
                                                        , loadingParams = LoadingParams scopesTrapScriptHash registriesSeedRef (Time.millisToPosix expiration)
                                                        }
                                                )
                                    )
                        )
            )


setupLastStep : Utxo.RefDict Output -> Scripts -> NetworkId -> Cip30.Wallet -> LastStepParams -> Result String ( SetupTxs, Loaded )
setupLastStep localStateUtxos scripts networkId connectedWallet { txs, rootUtxo, scopeOwners, scopesPermissions, scopesRegistries, contingencyPermissions, contingencyRegistry, loadingParams } =
    let
        scopesTreasuryScriptsResult : Result String (Scopes PlutusScript)
        scopesTreasuryScriptsResult =
            Scopes.toResult <|
                Scopes.map2 (\( registryHash, _ ) ( permissionsHash, _ ) -> setupTreasury loadingParams.expiration registryHash permissionsHash scripts.sundaeTreasury)
                    scopesRegistries
                    scopesPermissions

        scopesTreasuriesResult : Result String (Scopes ( Bytes CredentialHash, PlutusScript ))
        scopesTreasuriesResult =
            Result.map (Scopes.map (\script -> ( Script.hash <| Script.Plutus script, script ))) scopesTreasuryScriptsResult

        contingencyTreasuryResult : Result String ( Bytes CredentialHash, PlutusScript )
        contingencyTreasuryResult =
            setupTreasury loadingParams.expiration (Tuple.first contingencyRegistry) (Tuple.first contingencyPermissions) scripts.sundaeTreasury
                |> Result.map (\script -> ( Script.hash <| Script.Plutus script, script ))

        createRegistriesTx : Scopes (Bytes CredentialHash) -> Bytes CredentialHash -> Result String TxFinalized
        createRegistriesTx scopesTreasuryHashes contingencyTreasuryHash =
            let
                walletAddress =
                    Cip30.walletChangeAddress connectedWallet

                twoAda =
                    Value.onlyLovelace <| N.fromSafeInt 2000000

                registryDatum treasuryHash =
                    SundaeTypes.registryToData
                        { treasury = Address.ScriptHash treasuryHash
                        , vendor = Address.ScriptHash <| Bytes.dummy 28 ""
                        }

                mintIntent treasuryHash ( registryHash, registryScript ) =
                    -- Spend 2 ada from the wallet
                    [ TxIntent.Spend <|
                        TxIntent.FromWallet
                            { address = walletAddress
                            , value = twoAda
                            , guaranteedUtxos = [ loadingParams.registriesSeedUtxo ]
                            }

                    -- Mint the registry token
                    , TxIntent.MintBurn
                        { policyId = registryHash
                        , assets = Bytes.Map.singleton SundaeTypes.registryTokenName I.one
                        , scriptWitness =
                            Witness.Plutus
                                { script = ( Script.plutusVersion registryScript, Witness.ByValue <| Script.cborWrappedBytes registryScript )
                                , redeemerData = \_ -> Data.List [] -- unused
                                , requiredSigners = [] -- no required signer for minting
                                }
                        }

                    -- Send 2 ada + policyId/assetName + scopesDatum to the scopes trap address
                    , TxIntent.SendToOutput <|
                        { address = Address.script networkId registryHash
                        , amount = Value.add twoAda <| Value.onlyToken registryHash SundaeTypes.registryTokenName N.one
                        , datumOption = Just <| Utxo.datumValueFromData <| registryDatum treasuryHash
                        , referenceScript = Nothing
                        }
                    ]
            in
            [ mintIntent scopesTreasuryHashes.ledger scopesRegistries.ledger
            , mintIntent scopesTreasuryHashes.consensus scopesRegistries.consensus
            , mintIntent scopesTreasuryHashes.mercenaries scopesRegistries.mercenaries
            , mintIntent scopesTreasuryHashes.marketing scopesRegistries.marketing
            , mintIntent contingencyTreasuryHash contingencyRegistry
            ]
                |> List.concat
                |> TxIntent.finalize localStateUtxos []
                |> Result.mapError TxIntent.errorToString

        scopesResult : Bytes TransactionId -> Transaction -> Scopes ( Bytes CredentialHash, PlutusScript ) -> Result String (Scopes Scope)
        scopesResult txId registryTx scopesTreasuries =
            Scopes.toResult <|
                Scopes.map4 (Scope.setup txId registryTx)
                    scopeOwners
                    (Scopes.map Tuple.first scopesRegistries)
                    scopesPermissions
                    scopesTreasuries

        contingencyScopeResult : Bytes TransactionId -> Transaction -> ( Bytes CredentialHash, PlutusScript ) -> Result String Scope
        contingencyScopeResult txId registryTx contingencyTreasury =
            Scope.setup txId registryTx (MultisigScript.AnyOf []) (Tuple.first contingencyRegistry) contingencyPermissions contingencyTreasury
    in
    Result.map2 Tuple.pair scopesTreasuriesResult contingencyTreasuryResult
        |> Result.andThen
            (\( scopesTreasuries, contingencyTreasury ) ->
                createRegistriesTx (Scopes.map Tuple.first scopesTreasuries) (Tuple.first contingencyTreasury)
                    |> Result.andThen
                        (\txFinalized ->
                            let
                                registriesTxId =
                                    Transaction.computeTxId txFinalized.tx

                                walletKeyHash =
                                    Address.extractPaymentCred (Cip30.walletChangeAddress connectedWallet)
                                        |> Maybe.andThen Address.extractCredentialKeyHash
                                        |> Maybe.withDefault (Bytes.dummy 28 "")

                                defaultSignerDescriptions : BytesMap CredentialHash String
                                defaultSignerDescriptions =
                                    [ [ ( walletKeyHash, "Fees and deposit provider" ) ]
                                    , MultisigScript.extractRequiredSigners scopeOwners.ledger
                                        |> List.map (\hash -> ( hash, "Ledger scope signer" ))
                                    , MultisigScript.extractRequiredSigners scopeOwners.consensus
                                        |> List.map (\hash -> ( hash, "Consensus scope signer" ))
                                    , MultisigScript.extractRequiredSigners scopeOwners.mercenaries
                                        |> List.map (\hash -> ( hash, "Mercenaries scope signer" ))
                                    , MultisigScript.extractRequiredSigners scopeOwners.marketing
                                        |> List.map (\hash -> ( hash, "Marketing scope signer" ))
                                    ]
                                        |> List.concat
                                        |> Bytes.Map.fromList

                                setupTxs =
                                    { scopes =
                                        { txId = txs.scopes.txId
                                        , tx = txs.scopes.finalized.tx
                                        , expectedSignatures = txs.scopes.finalized.expectedSignatures
                                        , signerDescriptions = defaultSignerDescriptions
                                        }
                                    , permissions =
                                        { txId = txs.permissions.txId
                                        , tx = txs.permissions.finalized.tx
                                        , expectedSignatures = txs.permissions.finalized.expectedSignatures
                                        , signerDescriptions = defaultSignerDescriptions
                                        }
                                    , registries =
                                        { txId = registriesTxId
                                        , tx = txFinalized.tx
                                        , expectedSignatures = txFinalized.expectedSignatures
                                        , signerDescriptions = defaultSignerDescriptions
                                        }
                                    }

                                loadedTreasuryResult =
                                    Result.map2 (Loaded rootUtxo loadingParams)
                                        (scopesResult registriesTxId txFinalized.tx scopesTreasuries)
                                        (contingencyScopeResult registriesTxId txFinalized.tx contingencyTreasury)
                            in
                            Result.map (Tuple.pair setupTxs) loadedTreasuryResult
                        )
            )


setupAmaruScopes : Utxo.RefDict Output -> PlutusScript -> NetworkId -> Cip30.Wallet -> Scopes MultisigScript -> Result String ( ( ( OutputReference, Output ), TxFinalized ), ( Bytes CredentialHash, PlutusScript ) )
setupAmaruScopes localStateUtxos scopesTrapScript networkId connectedWallet scopeOwners =
    let
        walletAddress =
            Cip30.walletChangeAddress connectedWallet

        rootSeedUtxo : Result String ( OutputReference, Output )
        rootSeedUtxo =
            pickSeedUtxo localStateUtxos walletAddress (N.fromSafeInt 3000000)

        applyAmaruScopesTrapScript : OutputReference -> Result String PlutusScript
        applyAmaruScopesTrapScript rootSeedRef =
            Uplc.applyParamsToScript [ Utxo.outputReferenceToData rootSeedRef ]
                scopesTrapScript

        twoAda =
            Value.onlyLovelace <| N.fromSafeInt 2000000

        createAmaruScopesTx : OutputReference -> ( Bytes CredentialHash, PlutusScript ) -> Result TxFinalizationError TxFinalized
        createAmaruScopesTx rootSeedRef ( scriptHash, plutusScript ) =
            let
                scopesTrapAddress =
                    Address.script networkId scriptHash

                ( policyId, assetName ) =
                    ( scriptHash, Bytes.fromText "amaru scopes" )

                scopesDatum =
                    Scopes.toList scopeOwners
                        |> List.map MultisigScript.toData
                        |> Data.Constr N.zero
                        |> Utxo.datumValueFromData
            in
            -- Spend 2 ada from the wallet
            [ TxIntent.Spend <|
                TxIntent.FromWallet
                    { address = walletAddress
                    , value = twoAda
                    , guaranteedUtxos = [ rootSeedRef ]
                    }

            -- Mint 1 policyId / assetName
            , TxIntent.MintBurn
                { policyId = policyId
                , assets = Bytes.Map.singleton assetName I.one
                , scriptWitness =
                    Witness.Plutus
                        { script = ( Script.plutusVersion plutusScript, Witness.ByValue <| Script.cborWrappedBytes plutusScript )
                        , redeemerData = \_ -> Data.List [] -- unused
                        , requiredSigners = [] -- no required signer for minting
                        }
                }

            -- Send 2 ada + policyId/assetName + scopesDatum to the scopes trap address
            , TxIntent.SendToOutput <|
                { address = scopesTrapAddress
                , amount = Value.add twoAda <| Value.onlyToken policyId assetName N.one
                , datumOption = Just scopesDatum
                , referenceScript = Nothing
                }
            ]
                |> TxIntent.finalize localStateUtxos []
    in
    rootSeedUtxo
        |> Result.andThen
            (\(( seedRef, _ ) as seedUtxo) ->
                applyAmaruScopesTrapScript seedRef
                    |> Result.andThen
                        (\plutusScript ->
                            let
                                scriptHash =
                                    Script.hash <| Script.Plutus plutusScript
                            in
                            createAmaruScopesTx seedRef ( scriptHash, plutusScript )
                                |> Result.map (\txFinalized -> ( ( seedUtxo, txFinalized ), ( scriptHash, plutusScript ) ))
                                |> Result.mapError TxIntent.errorToString
                        )
            )


setupPermissions : Utxo.RefDict Output -> PlutusScript -> Cip30.Wallet -> Bytes CredentialHash -> Result String ( TxFinalized, Scopes ( Bytes CredentialHash, PlutusScript ), ( Bytes CredentialHash, PlutusScript ) )
setupPermissions localStateUtxos scopePermissionsScript connectedWallet scopesTrapScriptHash =
    let
        applyPermissionsScript scopeIndex =
            Uplc.applyParamsToScript
                [ Data.Bytes <| Bytes.toAny scopesTrapScriptHash
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                scopePermissionsScript
                |> Result.map (\script -> ( Script.hash <| Script.Plutus script, script ))

        scopesPermissionsResult : Result String (Scopes ( Bytes CredentialHash, PlutusScript ))
        scopesPermissionsResult =
            Scopes 0 1 2 3
                |> Scopes.map applyPermissionsScript
                |> Scopes.toResult

        createPermissionsStakeRegTx : List ( Bytes CredentialHash, PlutusScript ) -> Result TxFinalizationError TxFinalized
        createPermissionsStakeRegTx permissions =
            let
                walletAddress =
                    Cip30.walletChangeAddress connectedWallet

                twoAda =
                    N.fromSafeInt 2000000

                stakeRegWitness hash script =
                    Witness.WithScript hash <|
                        Witness.Plutus
                            { script = ( Script.plutusVersion script, Witness.ByValue <| Script.cborWrappedBytes script )
                            , redeemerData = \_ -> Data.List [] -- unused anyway
                            , requiredSigners = []
                            }

                stakeRegIntent ( hash, script ) =
                    [ TxIntent.Spend <|
                        TxIntent.FromWallet
                            { address = walletAddress
                            , value = Value.onlyLovelace twoAda
                            , guaranteedUtxos = []
                            }
                    , TxIntent.IssueCertificate <|
                        TxIntent.RegisterStake { delegator = stakeRegWitness hash script, deposit = twoAda }
                    ]
            in
            permissions
                |> List.concatMap stakeRegIntent
                |> TxIntent.finalize localStateUtxos []
    in
    Result.map2 Tuple.pair scopesPermissionsResult (applyPermissionsScript 4)
        |> Result.andThen
            (\( scopePermissions, contingencyPermissions ) ->
                createPermissionsStakeRegTx (Scopes.toList scopePermissions ++ [ contingencyPermissions ])
                    |> Result.mapError TxIntent.errorToString
                    |> Result.map (\txFinalized -> ( txFinalized, scopePermissions, contingencyPermissions ))
            )


setupRegistries : PlutusScript -> OutputReference -> Result String ( Scopes ( Bytes CredentialHash, PlutusScript ), ( Bytes CredentialHash, PlutusScript ) )
setupRegistries registryTrapScript registriesSeedUtxo =
    let
        applyRegistryScript scopeIndex =
            Uplc.applyParamsToScript
                [ Utxo.outputReferenceToData registriesSeedUtxo
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                registryTrapScript

        andComputeHash plutusScript =
            ( Script.hash <| Script.Plutus plutusScript, plutusScript )

        scopesRegistries =
            Scopes 0 1 2 3
                |> Scopes.map (applyRegistryScript >> Result.map andComputeHash)
                |> Scopes.toResult

        contingencyRegistry =
            applyRegistryScript 4 |> Result.map andComputeHash
    in
    Result.map2 Tuple.pair scopesRegistries contingencyRegistry


setupTreasury : Posix -> Bytes CredentialHash -> Bytes CredentialHash -> PlutusScript -> Result String PlutusScript
setupTreasury expiration registryScriptHash permissionsScriptHash sundaeTreasuryScript =
    let
        multisig =
            MultisigScript.Script permissionsScriptHash

        treasuryConfig : SundaeTypes.TreasuryConfiguration
        treasuryConfig =
            { registryToken = registryScriptHash
            , permissions =
                { reorganize = multisig
                , sweep = multisig
                , fund = MultisigScript.AnyOf []
                , disburse = multisig
                }
            , expiration = N.fromSafeInt <| Time.posixToMillis expiration
            , payoutUpperbound = N.zero
            }
    in
    Treasury.Sundae.initializeScript treasuryConfig sundaeTreasuryScript


pickSeedUtxo : Utxo.RefDict Output -> Address -> Natural -> Result String ( OutputReference, Output )
pickSeedUtxo localStateUtxos address amount =
    let
        utxosAtAddress : List ( OutputReference, Output )
        utxosAtAddress =
            Dict.Any.filter (\_ output -> output.address == address) localStateUtxos
                |> Dict.Any.toList
    in
    CoinSelection.collateral
        { availableUtxos = utxosAtAddress
        , allowedAddresses = Address.dictFromList [ ( address, () ) ]
        , targetAmount = amount
        }
        |> Result.mapError CoinSelection.errorToString
        |> Result.andThen
            (\{ selectedUtxos } ->
                case ( List.head selectedUtxos, List.head utxosAtAddress ) of
                    ( Nothing, Nothing ) ->
                        Err <| "There is no UTxO in the connected wallet for its default change address: " ++ Address.toBech32 address

                    ( Nothing, Just first ) ->
                        Ok first

                    ( Just first, _ ) ->
                        Ok first
            )



-- View ##############################################################


type alias ViewContext msg =
    { toMsg : Msg -> msg
    , tryBuildSetupTxs : msg
    }


viewForm : ViewContext msg -> Form -> Html msg
viewForm ctx form =
    case form.validation of
        Nothing ->
            viewTreasurySetupFormHelper ctx form Nothing

        Just (Err errors) ->
            viewTreasurySetupFormHelper ctx form (Just errors)

        Just (Ok { expiration, scopeOwners }) ->
            viewScopeOwnersSetup ctx expiration scopeOwners


viewTreasurySetupFormHelper : ViewContext msg -> Form -> Maybe String -> Html msg
viewTreasurySetupFormHelper ctx form errors =
    div []
        [ Html.h2 [] [ text "Setup New Treasury" ]
        , Html.p []
            [ Html.label [] [ text "Expiration date (Posix): " ]
            , Html.input
                [ HA.type_ "number"
                , HA.value <| String.fromInt form.expiration
                , HE.onInput (ctx.toMsg << ExpirationField)
                ]
                []
            , text <| " (" ++ displayPosixDate (Time.millisToPosix form.expiration) ++ ")"
            ]
        , div []
            [ Html.h3 [] [ text "Scope Owners" ]
            , Html.p [] [ text "Enter the aicone multisigs for each scope owner (for now just enter a public key hash):" ]
            ]
        , Html.map ctx.toMsg <|
            div []
                [ viewFormField "Ledger" form.ledgerOwner LedgerOwnerField
                , viewFormField "Consensus" form.consensusOwner ConsensusOwnerField
                , viewFormField "Mercenaries" form.mercenariesOwner MercenariesOwnerField
                , viewFormField "Marketing" form.marketingOwner MarketingOwnerField
                ]
        , Html.p []
            [ Html.button [ onClick (ctx.toMsg ValidateSetupForm) ]
                [ text "Validate scope owners & prepare Txs" ]
            ]
        , viewError errors
        ]


viewFormField : String -> String -> (String -> Msg) -> Html Msg
viewFormField label value fieldMsg =
    div []
        [ Html.label [] [ text <| label ++ ": " ]
        , Html.input
            [ HA.type_ "text"
            , HA.placeholder "Enter public key hash"
            , HE.onInput fieldMsg
            , HA.value value
            ]
            []
        ]


viewScopeOwnersSetup : ViewContext msg -> Int -> Scopes MultisigScript -> Html msg
viewScopeOwnersSetup ctx expiration scopeOwners =
    div []
        [ Html.h2 [] [ text "Setup New Treasury" ]
        , Html.p [] [ text <| "Expiration date (Posix): " ++ String.fromInt expiration ++ " (" ++ displayPosixDate (Time.millisToPosix expiration) ++ ")" ]
        , Html.h3 [] [ text "Scope Owners" ]
        , Html.ul []
            [ Html.li [] [ text <| "ledger: " ++ Debug.toString scopeOwners.ledger ]
            , Html.li [] [ text <| "consensus: " ++ Debug.toString scopeOwners.consensus ]
            , Html.li [] [ text <| "mercenaries: " ++ Debug.toString scopeOwners.mercenaries ]
            , Html.li [] [ text <| "marketing: " ++ Debug.toString scopeOwners.marketing ]
            ]
        , Html.p []
            [ Html.button [ onClick ctx.tryBuildSetupTxs ]
                [ text "Build the Txs" ]
            ]
        ]
