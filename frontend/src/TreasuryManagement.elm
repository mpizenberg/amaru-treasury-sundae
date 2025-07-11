module TreasuryManagement exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.MultiAsset as MultiAsset exposing (PolicyId)
import Cardano.Script as Script exposing (NativeScript(..), PlutusScript, PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxFinalizationError, TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Cardano.Witness as Witness
import Dict.Any
import Integer as I
import Json.Encode as JE
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Page.SignTx as SignTx
import RemoteData exposing (RemoteData)
import Time exposing (Posix)
import Treasury
import Types



-- MODEL


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , registryTrap : PlutusScript
    , scopesTrap : PlutusScript
    , scopePermissions : PlutusScript
    }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasurySetupForm SetupForm
    | TreasurySetupTxs SetupTxsState
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


type alias SetupForm =
    { ledgerOwner : String
    , consensusOwner : String
    , mercenariesOwner : String
    , marketingOwner : String
    , expiration : Int
    , validation : Maybe SetupFormValidation
    }


type alias SetupFormValidation =
    Result String { expiration : Int, scopeOwners : Scopes MultisigScript }


initTreasurySetupForm : Posix -> SetupForm
initTreasurySetupForm currentTime =
    { ledgerOwner = ""
    , consensusOwner = ""
    , mercenariesOwner = ""
    , marketingOwner = ""
    , expiration = Time.posixToMillis currentTime
    , validation = Nothing
    }


type alias SetupTxsState =
    { txs : SetupTxs
    , treasury : LoadedTreasury
    , tracking :
        { scopes : TxState
        , permissions : TxState
        , registries : TxState
        }
    }


initSetupTxsState : SetupTxs -> LoadedTreasury -> SetupTxsState
initSetupTxsState txs treasury =
    { txs = txs
    , treasury = treasury
    , tracking =
        { scopes = TxNotSubmittedYet
        , permissions = TxNotSubmittedYet
        , registries = TxNotSubmittedYet
        }
    }


type TxState
    = TxNotSubmittedYet
    | TxSubmitting
    | TxSubmitted


type alias LoadingTreasury =
    { loadingParams : LoadingParams
    , rootUtxo : RemoteData String ( OutputReference, Output )
    , scopes : Scopes LoadingScope
    , contingency : LoadingScope
    }


type alias LoadingParams =
    { pragmaScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : OutputReference
    , expiration : Posix
    }


encodeLoadingParams : LoadingParams -> JE.Value
encodeLoadingParams { pragmaScriptHash, registriesSeedUtxo, expiration } =
    JE.object
        [ ( "pragmaScriptHash", JE.string <| Bytes.toHex pragmaScriptHash )
        , ( "registriesSeedUtxo"
          , JE.object
                [ ( "transactionId", JE.string <| Bytes.toHex registriesSeedUtxo.transactionId )
                , ( "outputIndex", JE.int registriesSeedUtxo.outputIndex )
                ]
          )
        , ( "treasuryConfigExpiration", JE.int <| Time.posixToMillis expiration )
        ]


type alias LoadingScope =
    { owner : Maybe MultisigScript
    , permissionsScriptApplied : ( Bytes CredentialHash, PlutusScript )
    , permissionsScriptPublished : RemoteData String (Maybe ( OutputReference, Output ))
    , sundaeTreasuryScriptApplied : RemoteData String ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptPublished : RemoteData String (Maybe ( OutputReference, Output ))
    , registryNftPolicyId : Bytes PolicyId
    , registryUtxo : RemoteData String ( OutputReference, Output )
    , treasuryUtxos : RemoteData String (Utxo.RefDict Output)
    , expiration : Posix
    }


initLoadingScope : LoadingParams -> Scripts -> Int -> Result String LoadingScope
initLoadingScope params unappliedScripts scopeIndex =
    let
        permissionsScriptResult =
            Uplc.applyParamsToScript
                [ Data.Bytes <| Bytes.toAny params.pragmaScriptHash
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedScripts.scopePermissions

        registryScriptResult =
            Uplc.applyParamsToScript
                [ Utxo.outputReferenceToData params.registriesSeedUtxo
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedScripts.registryTrap

        loadingScope permissionsScript registryScript =
            { owner = Nothing
            , permissionsScriptApplied = ( Script.hash <| Script.Plutus permissionsScript, permissionsScript )
            , permissionsScriptPublished = RemoteData.Loading
            , sundaeTreasuryScriptApplied = RemoteData.Loading
            , sundaeTreasuryScriptPublished = RemoteData.Loading
            , registryNftPolicyId = Script.hash <| Script.Plutus registryScript
            , registryUtxo = RemoteData.Loading
            , treasuryUtxos = RemoteData.NotAsked
            , expiration = params.expiration
            }
    in
    Result.map2 loadingScope
        permissionsScriptResult
        registryScriptResult


setupAmaruTreasury : Utxo.RefDict Output -> Scripts -> NetworkId -> Cip30.Wallet -> { expiration : Int, scopeOwners : Scopes MultisigScript } -> Result String ( SetupTxs, LoadedTreasury )
setupAmaruTreasury localStateUtxos scripts networkId connectedWallet { expiration, scopeOwners } =
    setupAmaruScopes localStateUtxos scripts.scopesTrap networkId connectedWallet scopeOwners
        |> Result.andThen
            (\( ( scopesSeedUtxo, setupScopesTx ), ( scopesTrapScriptHash, scopesTrapScript ) ) ->
                let
                    scopesTxId =
                        Transaction.computeTxId setupScopesTx.tx

                    localStateAfterAmaruScopes =
                        (TxIntent.updateLocalState scopesTxId setupScopesTx.tx localStateUtxos).updatedState
                in
                setupPermissions localStateAfterAmaruScopes scripts.scopePermissions connectedWallet ( setupScopesTx, scopesTrapScriptHash, scopesTrapScript )
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


setupLastStep : Utxo.RefDict Output -> Scripts -> NetworkId -> Cip30.Wallet -> LastStepParams -> Result String ( SetupTxs, LoadedTreasury )
setupLastStep localStateUtxos scripts networkId connectedWallet { txs, rootUtxo, scopeOwners, scopesPermissions, scopesRegistries, contingencyPermissions, contingencyRegistry, loadingParams } =
    let
        scopesTreasuryScriptsResult : Result String (Scopes PlutusScript)
        scopesTreasuryScriptsResult =
            scopesToResult <|
                scopesMap2 (\( registryHash, _ ) ( permissionsHash, _ ) -> setupTreasury loadingParams.expiration registryHash permissionsHash scripts.sundaeTreasury)
                    scopesRegistries
                    scopesPermissions

        scopesTreasuriesResult : Result String (Scopes ( Bytes CredentialHash, PlutusScript ))
        scopesTreasuriesResult =
            Result.map (scopesMap (\script -> ( Script.hash <| Script.Plutus script, script ))) scopesTreasuryScriptsResult

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
                    Types.registryToData
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
                        , assets = Bytes.Map.singleton Types.registryTokenName I.one
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
                        , amount = Value.add twoAda <| Value.onlyToken registryHash Types.registryTokenName N.one
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
            scopesToResult <|
                scopesMap4 (setupScope txId registryTx)
                    scopeOwners
                    (scopesMap Tuple.first scopesRegistries)
                    scopesPermissions
                    scopesTreasuries

        contingencyScopeResult : Bytes TransactionId -> Transaction -> ( Bytes CredentialHash, PlutusScript ) -> Result String Scope
        contingencyScopeResult txId registryTx contingencyTreasury =
            setupScope txId registryTx (MultisigScript.AnyOf []) (Tuple.first contingencyRegistry) contingencyPermissions contingencyTreasury
    in
    Result.map2 Tuple.pair scopesTreasuriesResult contingencyTreasuryResult
        |> Result.andThen
            (\( scopesTreasuries, contingencyTreasury ) ->
                createRegistriesTx (scopesMap Tuple.first scopesTreasuries) (Tuple.first contingencyTreasury)
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
                                    Result.map2 (LoadedTreasury rootUtxo loadingParams)
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
                    scopesToList scopeOwners
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


setupPermissions : Utxo.RefDict Output -> PlutusScript -> Cip30.Wallet -> ( TxFinalized, Bytes CredentialHash, PlutusScript ) -> Result String ( TxFinalized, Scopes ( Bytes CredentialHash, PlutusScript ), ( Bytes CredentialHash, PlutusScript ) )
setupPermissions localStateUtxos scopePermissionsScript connectedWallet ( { tx }, scopesTrapScriptHash, scopesTrapScript ) =
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
                |> scopesMap applyPermissionsScript
                |> scopesToResult

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
                createPermissionsStakeRegTx (scopesToList scopePermissions ++ [ contingencyPermissions ])
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
                |> scopesMap (applyRegistryScript >> Result.map andComputeHash)
                |> scopesToResult

        contingencyRegistry =
            applyRegistryScript 4 |> Result.map andComputeHash
    in
    Result.map2 Tuple.pair scopesRegistries contingencyRegistry


setupTreasury : Posix -> Bytes CredentialHash -> Bytes CredentialHash -> PlutusScript -> Result String PlutusScript
setupTreasury expiration registryScriptHash permissionsScriptHash sundaeTreasuryScript =
    let
        multisig =
            MultisigScript.Script permissionsScriptHash

        treasuryConfig : Types.TreasuryConfiguration
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
    Treasury.initializeScript treasuryConfig sundaeTreasuryScript


setupScope : Bytes TransactionId -> Transaction -> MultisigScript -> Bytes CredentialHash -> ( Bytes CredentialHash, PlutusScript ) -> ( Bytes CredentialHash, PlutusScript ) -> Result String Scope
setupScope txId registryTx scopeOwner registryScriptHash permissions treasury =
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


{-| Picking a seed UTxO with a similar algorithm than the one used
to select viable collateral UTxOs, because it’s usually relatively clean UTxOs.
Default to the first UTxO at the address if the coin selection failed.
-}
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


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , loadingParams : LoadingParams
    , scopes : Scopes Scope
    , contingency : Scope
    }


type alias Scopes a =
    { ledger : a
    , consensus : a
    , mercenaries : a
    , marketing : a
    }


scopesToList : Scopes a -> List a
scopesToList { ledger, consensus, mercenaries, marketing } =
    [ ledger, consensus, mercenaries, marketing ]


scopesToResult : Scopes (Result err a) -> Result err (Scopes a)
scopesToResult { ledger, consensus, mercenaries, marketing } =
    Result.map4 Scopes ledger consensus mercenaries marketing


scopesMap : (a -> b) -> Scopes a -> Scopes b
scopesMap f { ledger, consensus, mercenaries, marketing } =
    { ledger = f ledger
    , consensus = f consensus
    , mercenaries = f mercenaries
    , marketing = f marketing
    }


scopesMap2 : (a -> b -> c) -> Scopes a -> Scopes b -> Scopes c
scopesMap2 f s1 s2 =
    { ledger = f s1.ledger s2.ledger
    , consensus = f s1.consensus s2.consensus
    , mercenaries = f s1.mercenaries s2.mercenaries
    , marketing = f s1.marketing s2.marketing
    }


scopesMap4 : (a -> b -> c -> d -> e) -> Scopes a -> Scopes b -> Scopes c -> Scopes d -> Scopes e
scopesMap4 f s1 s2 s3 s4 =
    { ledger = f s1.ledger s2.ledger s3.ledger s4.ledger
    , consensus = f s1.consensus s2.consensus s3.consensus s4.consensus
    , mercenaries = f s1.mercenaries s2.mercenaries s3.mercenaries s4.mercenaries
    , marketing = f s1.marketing s2.marketing s3.marketing s4.marketing
    }


type alias Scope =
    { owner : MultisigScript
    , permissionsScript : ( Bytes CredentialHash, PlutusScript )
    , permissionsScriptRef : Maybe ( OutputReference, Output )
    , sundaeTreasuryScript : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptRef : Maybe ( OutputReference, Output )
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , treasuryUtxos : Utxo.RefDict Output
    }


type TreasuryAction
    = NoTreasuryAction
    | MergeTreasuryUtxos ScopeActionState
    | Disburse ScopeActionState DisburseForm


type alias ScopeActionState =
    { scopeName : String
    , scope : Scope
    , rootUtxo : OutputReference
    , status : ActionStatus
    }


type ActionStatus
    = ActionIdle
    | BuildingFailure String
    | AwaitingSignature (Bytes TransactionId) TxFinalized


type alias DisburseForm =
    { selectedUtxo : ( OutputReference, Output )
    , selectedScopeOwners : List ( String, MultisigScript, Bool )
    , recipients : List { address : String, value : Natural }
    , error : Maybe String
    }


type alias TreasuryLoadingParamsForm =
    { pragmaScriptHash : String
    , registriesSeedUtxo : { transactionId : String, outputIndex : Int }
    , treasuryConfigExpiration : Int
    , error : Maybe String
    }


validateLoadingParams : TreasuryLoadingParamsForm -> Result String LoadingParams
validateLoadingParams formParams =
    case ( Bytes.fromHex formParams.pragmaScriptHash, Bytes.fromHex formParams.registriesSeedUtxo.transactionId ) of
        ( Nothing, _ ) ->
            Err <| "Pragma script hash is not a valid hex string: " ++ formParams.pragmaScriptHash

        ( _, Nothing ) ->
            Err <| "Registries seed utxo transaction id is not a valid hex string: " ++ formParams.registriesSeedUtxo.transactionId

        ( Just scriptHash, Just transactionId ) ->
            if Bytes.width scriptHash /= 28 then
                Err <| "Pragma script hash is " ++ String.fromInt (Bytes.width scriptHash) ++ " bytes long, but expected 28"

            else if Bytes.width transactionId /= 32 then
                Err <| "Registries seed utxo transaction id is " ++ String.fromInt (Bytes.width transactionId) ++ " bytes long, but expected 32"

            else
                Ok
                    { pragmaScriptHash = scriptHash
                    , registriesSeedUtxo = OutputReference transactionId formParams.registriesSeedUtxo.outputIndex
                    , expiration = Time.millisToPosix formParams.treasuryConfigExpiration
                    }


initLoadingTreasury : Scripts -> TreasuryLoadingParamsForm -> Result String LoadingTreasury
initLoadingTreasury unappliedScripts formParams =
    validateLoadingParams formParams
        |> Result.andThen
            (\params ->
                let
                    initLoadingScopeWithIndex index =
                        initLoadingScope params unappliedScripts index

                    loadingTreasury ledger consensus mercenaries marketing contingency =
                        { loadingParams = params
                        , rootUtxo = RemoteData.Loading
                        , scopes = Scopes ledger consensus mercenaries marketing
                        , contingency = contingency
                        }
                in
                Result.map5 loadingTreasury
                    (initLoadingScopeWithIndex 0)
                    (initLoadingScopeWithIndex 1)
                    (initLoadingScopeWithIndex 2)
                    (initLoadingScopeWithIndex 3)
                    -- contingency
                    (initLoadingScopeWithIndex 4)
            )


reload : Posix -> Scope -> LoadingScope
reload expiration scope =
    { owner = Just scope.owner
    , permissionsScriptApplied = scope.permissionsScript
    , permissionsScriptPublished = RemoteData.Success scope.permissionsScriptRef
    , sundaeTreasuryScriptApplied = RemoteData.Success scope.sundaeTreasuryScript
    , sundaeTreasuryScriptPublished = RemoteData.Success scope.sundaeTreasuryScriptRef
    , registryNftPolicyId =
        -- The registry UTxO is stored at the registry script address (policy ID)
        (Tuple.second scope.registryUtxo).address
            |> Address.extractPaymentCred
            |> Maybe.map Address.extractCredentialHash
            |> Maybe.withDefault (Bytes.dummy 28 "")
    , registryUtxo = RemoteData.Success scope.registryUtxo
    , treasuryUtxos = RemoteData.Success scope.treasuryUtxos
    , expiration = expiration
    }


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
    Treasury.reorganize
        { scriptWitnessSource = treasuryWitnessSource
        , registryOutputRef = Tuple.first scope.registryUtxo
        , additionalOutputRefs = [ rootUtxo ]
        , requiredSigners = requiredSigners
        , validityRange = validityRange
        , requiredWithdrawals = requiredWithdrawals
        , spentUtxos = utxos
        , receivers = \value -> [ TxIntent.SendTo scopeTreasuryAddress value ]
        }
