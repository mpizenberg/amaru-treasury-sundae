port module Main exposing (main)

import Api exposing (ProtocolParams)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxFinalizationError, TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import ConcurrentTask exposing (ConcurrentTask)
import Dict
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events as HE exposing (onClick)
import Http
import Integer as I
import Json.Decode as JD
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Page.SignTx as SignTx exposing (Subject(..))
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData)
import Result.Extra
import Task
import Time exposing (Posix)
import Treasury exposing (SpendConfig)
import Types


type alias Flags =
    { url : String
    , db : JD.Value
    , blueprints : List JD.Value
    , treasuryLoadingParams : TreasuryLoadingParams
    }


type alias TreasuryLoadingParams =
    { pragmaScriptHash : String
    , registriesSeedUtxo : { transactionId : String, outputIndex : Int }
    , treasuryConfigExpiration : Int
    }


main : Program Flags Model Msg
main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ fromWallet WalletMsg

                    -- , onUrlChange (locationHrefToRoute >> UrlChanged)
                    , onUrlChange UrlChanged
                    , ConcurrentTask.onProgress
                        { send = sendTask
                        , receive = receiveTask
                        , onProgress = OnTaskProgress
                        }
                        model.taskPool
                    ]
        , view = view
        }


port toWallet : JD.Value -> Cmd msg


port fromWallet : (JD.Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port sendTask : JD.Value -> Cmd msg


port receiveTask : (JD.Value -> msg) -> Sub msg


type Msg
    = NoMsg
    | GoToHomePage
    | UrlChanged String
    | WalletMsg JD.Value
    | DisconnectWallet
    | ConnectButtonClicked { id : String }
    | GotNetworkParams (Result Http.Error ProtocolParams)
    | SignTxButtonClicked SignTx.Subject SignTx.Prep
      -- Signature page
    | SignTxMsg SignTx.Msg
      -- Treasury management
    | StartTreasurySetup
    | StartTreasurySetupWithCurrentTime Posix
    | UpdateSetupForm SetupFormMsg
    | TreasuryLoadingParamsMsg TreasuryLoadingParamsMsg
    | StartTreasuryLoading
    | TreasuryMergingMsg TreasuryMergingMsg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = LoadedPragmaUtxo ( OutputReference, Output )
    | LoadedRegistryUtxos ( Scopes ( OutputReference, Output ), ( OutputReference, Output ) )
    | LoadedTreasuriesUtxos ( Scopes (Utxo.RefDict Output), Utxo.RefDict Output )



-- MODEL


type alias Model =
    { taskPool : ConcurrentTask.Pool Msg String TaskCompleted
    , db : JD.Value
    , page : Page
    , networkId : NetworkId
    , protocolParams : ProtocolParams
    , discoveredWallets : List Cip30.WalletDescriptor
    , connectedWallet : Maybe Cip30.Wallet
    , localStateUtxos : Utxo.RefDict Output
    , scripts : Scripts
    , treasuryLoadingParams : TreasuryLoadingParams
    , treasuryManagement : TreasuryManagement
    , treasuryAction : TreasuryAction
    , error : Maybe String
    }


initialModel : JD.Value -> Scripts -> TreasuryLoadingParams -> Model
initialModel db scripts treasuryLoadingParams =
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , scripts = scripts
    , treasuryLoadingParams = treasuryLoadingParams
    , treasuryManagement = TreasuryUnspecified
    , treasuryAction = NoTreasuryAction
    , error = Nothing
    }


type Page
    = Home
    | SignTxPage SignTx.Model


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
    { pragmaScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : OutputReference
    , treasuryConfigExpiration : Natural
    , rootUtxo : RemoteData String ( OutputReference, Output )
    , expiration : Posix
    , scopes : Scopes LoadingScope
    , contingency : LoadingScope
    }


initLoadingTreasury : PlutusScript -> PlutusScript -> String -> { transactionId : String, outputIndex : Int } -> Int -> Result String LoadingTreasury
initLoadingTreasury unappliedScopePermissionScript unappliedRegistryTrapScript pragmaScriptHashHex { transactionId, outputIndex } expiration =
    let
        pragmaScriptHash =
            Bytes.fromHexUnchecked pragmaScriptHashHex

        registriesSeedUtxo =
            OutputReference (Bytes.fromHexUnchecked transactionId) outputIndex

        treasuryConfigExpiration =
            N.fromSafeInt expiration

        initLoadingScopeWithIndex index =
            initLoadingScope
                unappliedScopePermissionScript
                pragmaScriptHash
                unappliedRegistryTrapScript
                registriesSeedUtxo
                treasuryConfigExpiration
                index

        loadingTreasury ledger consensus mercenaries marketing contingency =
            { pragmaScriptHash = pragmaScriptHash
            , registriesSeedUtxo = registriesSeedUtxo
            , treasuryConfigExpiration = treasuryConfigExpiration
            , rootUtxo = RemoteData.Loading
            , expiration = Time.millisToPosix expiration
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


type alias LoadingScope =
    { owner : Maybe MultisigScript
    , permissionsScriptApplied : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptApplied : RemoteData String ( Bytes CredentialHash, PlutusScript )
    , registryNftPolicyId : Bytes PolicyId
    , registryUtxo : RemoteData String ( OutputReference, Output )
    , treasuryUtxos : RemoteData String (Utxo.RefDict Output)
    , expiration : Natural
    }


initLoadingScope : PlutusScript -> Bytes CredentialHash -> PlutusScript -> OutputReference -> Natural -> Int -> Result String LoadingScope
initLoadingScope unappliedScopePermissionScript pragmaScriptHash unappliedRegistryTrapScript registriesSeedUtxo treasuryConfigExpiration scopeIndex =
    let
        permissionsScriptResult =
            Uplc.applyParamsToScript
                [ Data.Bytes <| Bytes.toAny pragmaScriptHash
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedScopePermissionScript

        registryScriptResult =
            Uplc.applyParamsToScript
                [ Utxo.outputReferenceToData registriesSeedUtxo
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                unappliedRegistryTrapScript

        loadingScope permissionsScript registryScript =
            { owner = Nothing
            , permissionsScriptApplied = ( Script.hash <| Script.Plutus permissionsScript, permissionsScript )
            , sundaeTreasuryScriptApplied = RemoteData.Loading
            , registryNftPolicyId = Script.hash <| Script.Plutus registryScript
            , registryUtxo = RemoteData.Loading
            , treasuryUtxos = RemoteData.NotAsked
            , expiration = treasuryConfigExpiration
            }
    in
    Result.map2 loadingScope
        permissionsScriptResult
        registryScriptResult


setupAmaruTreasury : Model -> Cip30.Wallet -> { expiration : Int, scopeOwners : Scopes MultisigScript } -> Result String ( SetupTxs, LoadedTreasury )
setupAmaruTreasury model connectedWallet { expiration, scopeOwners } =
    setupAmaruScopes model connectedWallet scopeOwners
        |> Result.andThen
            (\( ( scopesSeedUtxo, setupScopesTx ), ( scopesTrapScriptHash, scopesTrapScript ) ) ->
                let
                    scopesTxId =
                        Transaction.computeTxId setupScopesTx.tx

                    localStateAfterAmaruScopes =
                        (TxIntent.updateLocalState scopesTxId setupScopesTx.tx model.localStateUtxos).updatedState

                    modelAfterAmaruScopesTx =
                        { model | localStateUtxos = localStateAfterAmaruScopes }
                in
                setupPermissions modelAfterAmaruScopesTx connectedWallet ( setupScopesTx, scopesTrapScriptHash, scopesTrapScript )
                    |> Result.andThen
                        (\( setupPermissionsTx, scopesPermissions, contingencyPermissions ) ->
                            let
                                permissionsTxId =
                                    Transaction.computeTxId setupPermissionsTx.tx

                                localStateAfterPermissions =
                                    (TxIntent.updateLocalState permissionsTxId setupPermissionsTx.tx modelAfterAmaruScopesTx.localStateUtxos).updatedState

                                modelAfterPermissionsTx =
                                    { model | localStateUtxos = localStateAfterPermissions }
                            in
                            pickSeedUtxo localStateAfterPermissions (Cip30.walletChangeAddress connectedWallet) (N.fromSafeInt 10000000)
                                |> Result.andThen
                                    (\( registriesSeedRef, registriesSeedOutput ) ->
                                        setupRegistries model.scripts.registryTrap registriesSeedRef
                                            |> Result.andThen
                                                (\( scopesRegistries, contingencyRegistry ) ->
                                                    setupLastStep modelAfterPermissionsTx
                                                        connectedWallet
                                                        { txs =
                                                            { scopes = { txId = scopesTxId, finalized = setupScopesTx }
                                                            , permissions = { txId = permissionsTxId, finalized = setupPermissionsTx }
                                                            }
                                                        , rootUtxo = scopesSeedUtxo
                                                        , expiration = Time.millisToPosix expiration
                                                        , scopeOwners = scopeOwners
                                                        , scopesPermissions = scopesPermissions
                                                        , scopesRegistries = scopesRegistries
                                                        , contingencyPermissions = contingencyPermissions
                                                        , contingencyRegistry = contingencyRegistry
                                                        , scopesScriptHash = scopesTrapScriptHash
                                                        , registriesSeedUtxo = ( registriesSeedRef, registriesSeedOutput )
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
    , expiration : Posix
    , scopeOwners : Scopes MultisigScript
    , scopesPermissions : Scopes ( Bytes CredentialHash, PlutusScript )
    , scopesRegistries : Scopes ( Bytes CredentialHash, PlutusScript )
    , contingencyPermissions : ( Bytes CredentialHash, PlutusScript )
    , contingencyRegistry : ( Bytes CredentialHash, PlutusScript )
    , scopesScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : ( OutputReference, Output )
    }


type alias SetupTxs =
    { scopes : SignTx.Prep
    , permissions : SignTx.Prep
    , registries : SignTx.Prep
    }


setupLastStep : Model -> Cip30.Wallet -> LastStepParams -> Result String ( SetupTxs, LoadedTreasury )
setupLastStep model connectedWallet { txs, rootUtxo, expiration, scopeOwners, scopesPermissions, scopesRegistries, contingencyPermissions, contingencyRegistry, scopesScriptHash, registriesSeedUtxo } =
    let
        scopesTreasuryScriptsResult : Result String (Scopes PlutusScript)
        scopesTreasuryScriptsResult =
            scopesToResult <|
                scopesMap2 (\( registryHash, _ ) ( permissionsHash, _ ) -> setupTreasury expiration registryHash permissionsHash model.scripts.sundaeTreasury)
                    scopesRegistries
                    scopesPermissions

        scopesTreasuriesResult : Result String (Scopes ( Bytes CredentialHash, PlutusScript ))
        scopesTreasuriesResult =
            Result.map (scopesMap (\script -> ( Script.hash <| Script.Plutus script, script ))) scopesTreasuryScriptsResult

        contingencyTreasuryResult : Result String ( Bytes CredentialHash, PlutusScript )
        contingencyTreasuryResult =
            setupTreasury expiration (Tuple.first contingencyRegistry) (Tuple.first contingencyPermissions) model.scripts.sundaeTreasury
                |> Result.map (\script -> ( Script.hash <| Script.Plutus script, script ))

        registriesSeedUtxoRef =
            Tuple.first registriesSeedUtxo

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
                        FromWallet
                            { address = walletAddress
                            , value = twoAda
                            , guaranteedUtxos = [ registriesSeedUtxoRef ]
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
                        { address = Address.script model.networkId registryHash
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
                |> TxIntent.finalize model.localStateUtxos []
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
                                    Result.map2 (LoadedTreasury rootUtxo scopesScriptHash registriesSeedUtxoRef expiration)
                                        (scopesResult registriesTxId txFinalized.tx scopesTreasuries)
                                        (contingencyScopeResult registriesTxId txFinalized.tx contingencyTreasury)
                            in
                            Result.map (Tuple.pair setupTxs) loadedTreasuryResult
                        )
            )


setupAmaruScopes : Model -> Cip30.Wallet -> Scopes MultisigScript -> Result String ( ( ( OutputReference, Output ), TxFinalized ), ( Bytes CredentialHash, PlutusScript ) )
setupAmaruScopes model connectedWallet scopeOwners =
    let
        walletAddress =
            Cip30.walletChangeAddress connectedWallet

        rootSeedUtxo : Result String ( OutputReference, Output )
        rootSeedUtxo =
            pickSeedUtxo model.localStateUtxos walletAddress (N.fromSafeInt 3000000)

        applyAmaruScopesTrapScript : OutputReference -> Result String PlutusScript
        applyAmaruScopesTrapScript rootSeedRef =
            Uplc.applyParamsToScript [ Utxo.outputReferenceToData rootSeedRef ]
                model.scripts.scopesTrap

        twoAda =
            Value.onlyLovelace <| N.fromSafeInt 2000000

        createAmaruScopesTx : OutputReference -> ( Bytes CredentialHash, PlutusScript ) -> Result TxFinalizationError TxFinalized
        createAmaruScopesTx rootSeedRef ( scriptHash, plutusScript ) =
            let
                scopesTrapAddress =
                    Address.script model.networkId scriptHash

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
                FromWallet
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
                |> TxIntent.finalize model.localStateUtxos []
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


setupPermissions : Model -> Cip30.Wallet -> ( TxFinalized, Bytes CredentialHash, PlutusScript ) -> Result String ( TxFinalized, Scopes ( Bytes CredentialHash, PlutusScript ), ( Bytes CredentialHash, PlutusScript ) )
setupPermissions model connectedWallet ( { tx }, scopesTrapScriptHash, scopesTrapScript ) =
    let
        applyPermissionsScript scopeIndex =
            Uplc.applyParamsToScript
                [ Data.Bytes <| Bytes.toAny scopesTrapScriptHash
                , Data.Constr (N.fromSafeInt scopeIndex) [] -- the scope Data representation
                ]
                model.scripts.scopePermissions
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
                        FromWallet
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
                |> TxIntent.finalize model.localStateUtxos []
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
                , sundaeTreasuryScript = treasury
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
    , scopesScriptHash : Bytes CredentialHash -- needed for loading
    , registriesSeedUtxoRef : OutputReference -- needed for loading
    , expiration : Posix -- needed for loading
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
    , sundaeTreasuryScript : ( Bytes CredentialHash, PlutusScript )
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , treasuryUtxos : Utxo.RefDict Output
    }


type TreasuryAction
    = NoTreasuryAction
    | MergeTreasuryUtxos MergeTreasuryUtxosState


type alias MergeTreasuryUtxosState =
    { scopeName : String
    , scope : Scope
    , rootUtxo : OutputReference
    , status : MergeStatus
    }


type MergeStatus
    = MergeIdle
    | MergeBuilding (Result TxFinalizationError TxFinalized)
    | MergeAwaitingSignature Transaction
    | MergeSubmitting Transaction


init : Flags -> ( Model, Cmd Msg )
init { db, blueprints, treasuryLoadingParams } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        registryBlueprint =
            List.Extra.find (\{ name } -> name == "traps.treasury_registry.spend") decodedBlueprints

        scopesBlueprint =
            List.Extra.find (\{ name } -> name == "traps.scopes.mint") decodedBlueprints

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        permissionsBlueprint =
            List.Extra.find (\{ name } -> name == "permissions.permissions.withdraw") decodedBlueprints

        ( scripts, blueprintError ) =
            case ( ( registryBlueprint, scopesBlueprint ), ( sundaeTreasuryBlueprint, permissionsBlueprint ) ) of
                ( ( Just registry, Just scopes ), ( Just treasury, Just permissions ) ) ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 treasury.scriptBytes
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 registry.scriptBytes
                      , scopesTrap = Script.plutusScriptFromBytes PlutusV3 scopes.scriptBytes
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 permissions.scriptBytes
                      }
                    , Nothing
                    )

                _ ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopesTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Just "Failed the retrieve some of the Plutus blueprints. Did you forget to build the aiken code?"
                    )

        model =
            initialModel db scripts treasuryLoadingParams
    in
    case blueprintError of
        Nothing ->
            ( model
            , Cmd.batch
                [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
                , Api.loadProtocolParams model.networkId GotNetworkParams
                ]
            )

        Just error ->
            ( { model | error = Just error }, Cmd.none )


blueprintDecoder : JD.Decoder (List ScriptBlueprint)
blueprintDecoder =
    JD.at [ "validators" ]
        (JD.list
            (JD.map4 ScriptBlueprint
                (JD.field "title" JD.string)
                (JD.field "compiledCode" JD.string |> JD.map Bytes.fromHexUnchecked)
                (JD.field "hash" JD.string |> JD.map Bytes.fromHexUnchecked)
                (JD.maybe (JD.field "parameters" JD.value) |> JD.map (\p -> Maybe.map (always True) p |> Maybe.withDefault False))
            )
        )


type alias ScriptBlueprint =
    { name : String
    , scriptBytes : Bytes ScriptCbor
    , hash : Bytes CredentialHash
    , hasParams : Bool
    }



-- UPDATE ############################################################


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        GoToHomePage ->
            ( { model | page = Home }, Cmd.none )

        UrlChanged _ ->
            ( Debug.todo "", Debug.todo "" )

        WalletMsg value ->
            handleWalletMsg value model

        DisconnectWallet ->
            ( { model | connectedWallet = Nothing }, Cmd.none )

        ConnectButtonClicked { id } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Just 3 })) )

        GotNetworkParams (Err httpError) ->
            ( { model | error = Just <| Debug.toString httpError }, Cmd.none )

        GotNetworkParams (Ok params) ->
            ( { model | protocolParams = params }, Cmd.none )

        SignTxButtonClicked signSubject signingPrep ->
            ( { model | page = SignTxPage <| SignTx.initialModel signSubject (Just signingPrep) }
            , Cmd.none
            )

        -- Signature page
        SignTxMsg pageMsg ->
            case model.page of
                SignTxPage pageModel ->
                    handleSignTxMsg pageModel pageMsg model

                _ ->
                    ( model, Cmd.none )

        -- Treasury management
        StartTreasurySetup ->
            ( model, Task.perform StartTreasurySetupWithCurrentTime Time.now )

        StartTreasurySetupWithCurrentTime currentTime ->
            ( { model | treasuryManagement = TreasurySetupForm <| initTreasurySetupForm currentTime }, Cmd.none )

        UpdateSetupForm subMsg ->
            case model.treasuryManagement of
                TreasurySetupForm form ->
                    handleTreasurySetupFormUpdate subMsg form model

                _ ->
                    ( model, Cmd.none )

        TreasuryLoadingParamsMsg paramsMsg ->
            handleTreasuryLoadingParamsMsg paramsMsg model

        StartTreasuryLoading ->
            startTreasuryLoading model

        TreasuryMergingMsg submsg ->
            handleTreasuryMergingMsg submsg model

        -- Task port
        OnTaskProgress ( taskPool, cmd ) ->
            ( { model | taskPool = taskPool }, cmd )

        OnTaskComplete taskCompleted ->
            handleTask taskCompleted model



-- Wallet


walletResponseDecoder : JD.Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.singleton 30 Cip30.apiDecoder


handleWalletMsg : JD.Value -> Model -> ( Model, Cmd Msg )
handleWalletMsg value model =
    case JD.decodeValue walletResponseDecoder value of
        -- We just discovered available wallets
        Ok (Cip30.AvailableWallets wallets) ->
            ( { model | discoveredWallets = wallets }, Cmd.none )

        -- We just connected to the wallet, let’s ask for the available utxos
        Ok (Cip30.EnabledWallet wallet) ->
            ( { model | connectedWallet = Just wallet }
            , toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
            )

        -- We just received the utxos, let’s add them to the local state
        Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)) ->
            ( { model | localStateUtxos = List.foldl (\( ref, output ) -> Dict.Any.insert ref output) model.localStateUtxos utxos }
            , Cmd.none
            )

        -- The wallet just signed a Tx
        Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeyWitnesses)) ->
            case model.page of
                SignTxPage pageModel ->
                    ( { model | page = SignTxPage <| SignTx.addWalletSignatures vkeyWitnesses pageModel }
                    , Cmd.none
                    )

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)) ->
            case model.page of
                SignTxPage pageModel ->
                    ( { model
                        | page = SignTxPage <| SignTx.recordSubmittedTx txId pageModel

                        -- Update local state UTxOs
                        , localStateUtxos =
                            SignTx.getTxInfo pageModel
                                |> Maybe.map (\{ tx } -> (TxIntent.updateLocalState txId tx model.localStateUtxos).updatedState)
                                |> Maybe.withDefault model.localStateUtxos
                        , treasuryManagement =
                            updateTreasuryManagementWithTx txId model.treasuryManagement
                      }
                    , Cmd.none
                    )

                -- No other page expects to submit a Tx
                _ ->
                    ( model, Cmd.none )

        Ok (Cip30.ApiResponse _ _) ->
            ( { model | error = Just "Unhandled CIP30 response yet" }
            , Cmd.none
            )

        -- Received an error message from the wallet
        Ok (Cip30.ApiError { info, code }) ->
            ( { model
                | page = resetSigningStep info model.page
                , error = Just <| "Wallet Error (code " ++ String.fromInt code ++ "):\n" ++ info
              }
            , Cmd.none
            )

        -- Unknown type of message received from the wallet
        Ok (Cip30.UnhandledResponseType error) ->
            ( { model | error = Just error }
            , Cmd.none
            )

        Err error ->
            ( { model | error = Just <| "CIP-30 decoding error: " ++ JD.errorToString error }
            , Cmd.none
            )


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SignTxPage pageModel ->
            SignTxPage <| SignTx.resetSubmission error pageModel

        _ ->
            page


updateTreasuryManagementWithTx : Bytes TransactionId -> TreasuryManagement -> TreasuryManagement
updateTreasuryManagementWithTx txId treasuryManagement =
    case treasuryManagement of
        TreasurySetupTxs setupState ->
            markTxAsSubmitted txId setupState
                |> upgradeToLoadedIfSetupIsDone

        _ ->
            treasuryManagement


markTxAsSubmitted : Bytes TransactionId -> SetupTxsState -> SetupTxsState
markTxAsSubmitted txId ({ tracking } as state) =
    if txId == state.txs.scopes.txId then
        { state | tracking = { tracking | scopes = TxSubmitted } }

    else if txId == state.txs.permissions.txId then
        { state | tracking = { tracking | permissions = TxSubmitted } }

    else if txId == state.txs.registries.txId then
        { state | tracking = { tracking | registries = TxSubmitted } }

    else
        state


upgradeToLoadedIfSetupIsDone : SetupTxsState -> TreasuryManagement
upgradeToLoadedIfSetupIsDone ({ tracking } as state) =
    case ( tracking.scopes, tracking.permissions, tracking.registries ) of
        ( TxSubmitted, TxSubmitted, TxSubmitted ) ->
            TreasuryFullyLoaded state.treasury

        _ ->
            TreasurySetupTxs state



-- Signature page


handleSignTxMsg : SignTx.Model -> SignTx.Msg -> Model -> ( Model, Cmd Msg )
handleSignTxMsg pageModel msg model =
    let
        ( walletSignTx, walletSubmitTx ) =
            case model.connectedWallet of
                Nothing ->
                    ( \_ -> Cmd.none
                    , \_ -> Cmd.none
                    )

                Just wallet ->
                    ( \tx -> toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = True } tx))
                    , \tx -> toWallet (Cip30.encodeRequest (Cip30.submitTx wallet tx))
                    )

        ctx =
            { wrapMsg = SignTxMsg
            , wallet = model.connectedWallet
            , walletSignTx = walletSignTx
            , walletSubmitTx = walletSubmitTx
            }
    in
    SignTx.update ctx msg pageModel
        |> Tuple.mapFirst (\newPageModel -> { model | page = SignTxPage newPageModel })



-- Initializing a new Treasury


type SetupFormMsg
    = LedgerOwnerField String
    | ConsensusOwnerField String
    | MercenariesOwnerField String
    | MarketingOwnerField String
    | ExpirationField String
    | ValidateSetupForm
    | TryBuildSetupTxs


handleTreasurySetupFormUpdate : SetupFormMsg -> SetupForm -> Model -> ( Model, Cmd Msg )
handleTreasurySetupFormUpdate msg form model =
    let
        updatedTreasuryManagement =
            updateSetupFormField model msg form
    in
    ( { model | treasuryManagement = updatedTreasuryManagement }
    , Cmd.none
    )


updateSetupFormField : Model -> SetupFormMsg -> SetupForm -> TreasuryManagement
updateSetupFormField model msg form =
    case msg of
        LedgerOwnerField value ->
            TreasurySetupForm { form | ledgerOwner = value }

        ConsensusOwnerField value ->
            TreasurySetupForm { form | consensusOwner = value }

        MercenariesOwnerField value ->
            TreasurySetupForm { form | mercenariesOwner = value }

        MarketingOwnerField value ->
            TreasurySetupForm { form | marketingOwner = value }

        ExpirationField value ->
            TreasurySetupForm { form | expiration = Maybe.withDefault 0 <| String.toInt value }

        ValidateSetupForm ->
            TreasurySetupForm { form | validation = Just <| validateSetup form }

        TryBuildSetupTxs ->
            case form.validation of
                Just (Ok scopeOwners) ->
                    case model.connectedWallet of
                        Nothing ->
                            TreasurySetupForm { form | validation = Just <| Err "Please connect wallet first" }

                        Just wallet ->
                            case setupAmaruTreasury model wallet scopeOwners of
                                Err error ->
                                    TreasurySetupForm { form | validation = Just <| Err error }

                                Ok ( txs, treasury ) ->
                                    TreasurySetupTxs <| initSetupTxsState txs treasury

                _ ->
                    TreasurySetupForm form


validateSetup : SetupForm -> SetupFormValidation
validateSetup form =
    Scopes form.ledgerOwner form.consensusOwner form.mercenariesOwner form.marketingOwner
        |> scopesMap validateKeyHash
        |> scopesToResult
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



-- Loading Treasury Form


type TreasuryLoadingParamsMsg
    = UpdatePragmaScriptHash String
    | UpdateRegistriesSeedTransactionId String
    | UpdateRegistriesSeedOutputIndex String
    | UpdateExpiration String


handleTreasuryLoadingParamsMsg : TreasuryLoadingParamsMsg -> Model -> ( Model, Cmd Msg )
handleTreasuryLoadingParamsMsg msg ({ treasuryLoadingParams } as model) =
    let
        newParams params =
            { model | treasuryLoadingParams = params }

        currentUtxo =
            treasuryLoadingParams.registriesSeedUtxo
    in
    case msg of
        UpdatePragmaScriptHash value ->
            ( newParams { treasuryLoadingParams | pragmaScriptHash = value }, Cmd.none )

        UpdateRegistriesSeedTransactionId value ->
            ( newParams { treasuryLoadingParams | registriesSeedUtxo = { currentUtxo | transactionId = value } }, Cmd.none )

        UpdateRegistriesSeedOutputIndex value ->
            case String.toInt value of
                Just outputIndex ->
                    ( newParams { treasuryLoadingParams | registriesSeedUtxo = { currentUtxo | outputIndex = outputIndex } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateExpiration value ->
            case String.toInt value of
                Just expiration ->
                    ( newParams { treasuryLoadingParams | treasuryConfigExpiration = expiration }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- Loading the Treasury


startTreasuryLoading : Model -> ( Model, Cmd Msg )
startTreasuryLoading model =
    let
        { pragmaScriptHash, registriesSeedUtxo, treasuryConfigExpiration } =
            model.treasuryLoadingParams

        -- Load Pragma UTxO
        ( updatedTaskPool, loadPragmaUtxoCmd ) =
            Api.retrieveAssetUtxo model.networkId (Bytes.fromHexUnchecked pragmaScriptHash) (Bytes.fromText "amaru scopes")
                |> ConcurrentTask.mapError Debug.toString
                |> ConcurrentTask.andThen (Api.retrieveOutput model.networkId)
                |> ConcurrentTask.map LoadedPragmaUtxo
                |> ConcurrentTask.attempt
                    { pool = model.taskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }

        loadingTreasuryResult =
            initLoadingTreasury
                model.scripts.scopePermissions
                model.scripts.registryTrap
                pragmaScriptHash
                registriesSeedUtxo
                treasuryConfigExpiration

        loadRegistryUtxos : Scopes LoadingScope -> LoadingScope -> ConcurrentTask String ( Scopes ( OutputReference, Output ), ( OutputReference, Output ) )
        loadRegistryUtxos loadingScopes contingencyScope =
            let
                registryAssetName =
                    Types.registryTokenName

                registryAssets =
                    Scopes
                        ( loadingScopes.ledger.registryNftPolicyId, registryAssetName )
                        ( loadingScopes.consensus.registryNftPolicyId, registryAssetName )
                        ( loadingScopes.mercenaries.registryNftPolicyId, registryAssetName )
                        ( loadingScopes.marketing.registryNftPolicyId, registryAssetName )

                contingencyAsset =
                    ( contingencyScope.registryNftPolicyId, registryAssetName )

                -- Query assets utxos
                assetsUtxosTask : ConcurrentTask String (MultiAsset (Utxo.RefDict Output))
                assetsUtxosTask =
                    Api.retrieveAssetsUtxos { db = model.db }
                        model.networkId
                        [ registryAssets.ledger
                        , registryAssets.consensus
                        , registryAssets.mercenaries
                        , registryAssets.marketing
                        , contingencyAsset
                        ]

                extractLoadedUtxos : MultiAsset (Utxo.RefDict Output) -> ConcurrentTask String ( Scopes ( OutputReference, Output ), ( OutputReference, Output ) )
                extractLoadedUtxos assetsUtxos =
                    Result.map5 (\s1 s2 s3 s4 contingency -> ( Scopes s1 s2 s3 s4, contingency ))
                        (extractRegistryUtxo registryAssets.ledger assetsUtxos)
                        (extractRegistryUtxo registryAssets.consensus assetsUtxos)
                        (extractRegistryUtxo registryAssets.mercenaries assetsUtxos)
                        (extractRegistryUtxo registryAssets.marketing assetsUtxos)
                        (extractRegistryUtxo contingencyAsset assetsUtxos)
                        |> ConcurrentTask.fromResult

                extractRegistryUtxo ( policyId, assetName ) multiAsset =
                    case MultiAsset.get policyId assetName multiAsset of
                        Nothing ->
                            Err <| "Missing asset: " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName

                        Just refDict ->
                            case Dict.Any.toList refDict of
                                [] ->
                                    Err <| "Missing asset: " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName

                                [ utxo ] ->
                                    Ok utxo

                                _ ->
                                    Err <| "Asset in more than 1 outputs! : " ++ Bytes.toHex policyId ++ " / " ++ Bytes.pretty assetName
            in
            ConcurrentTask.andThen extractLoadedUtxos assetsUtxosTask

        attemptLoadRegistryUtxosTask loadingScopes contingencyScope =
            loadRegistryUtxos loadingScopes contingencyScope
                |> ConcurrentTask.map LoadedRegistryUtxos
                |> ConcurrentTask.attempt
                    { pool = updatedTaskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }
    in
    case loadingTreasuryResult of
        Ok loadingTreasury ->
            let
                ( updatedAgainTaskPool, loadRegistryUtxosCmd ) =
                    attemptLoadRegistryUtxosTask loadingTreasury.scopes loadingTreasury.contingency
            in
            ( { model
                | taskPool = updatedAgainTaskPool
                , treasuryManagement = TreasuryLoading loadingTreasury
              }
            , Cmd.batch
                [ loadPragmaUtxoCmd
                , loadRegistryUtxosCmd
                ]
            )

        Err error ->
            ( { model | error = Just error }, Cmd.none )



-- Merge UTxOs


type TreasuryMergingMsg
    = StartMergeUtxos String Scope OutputReference
    | BuildMergeTransaction (List (Bytes CredentialHash))
    | BuildMergeTransactionWithTime (List (Bytes CredentialHash)) Posix
    | SignMergeTransaction Transaction
    | SubmitMergeTransaction Transaction
    | CancelMergeAction


handleTreasuryMergingMsg : TreasuryMergingMsg -> Model -> ( Model, Cmd Msg )
handleTreasuryMergingMsg msg model =
    case msg of
        StartMergeUtxos scopeName scope rootUtxo ->
            handleStartMergeUtxos scopeName scope rootUtxo model

        BuildMergeTransaction requiredSigners ->
            ( model
            , Task.perform (TreasuryMergingMsg << BuildMergeTransactionWithTime requiredSigners) Time.now
            )

        BuildMergeTransactionWithTime requiredSigners currentTime ->
            handleBuildMergeTransaction model requiredSigners currentTime

        SignMergeTransaction transaction ->
            handleSignMergeTransaction transaction model

        SubmitMergeTransaction transaction ->
            handleSubmitMergeTransaction transaction model

        CancelMergeAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none )


handleStartMergeUtxos : String -> Scope -> OutputReference -> Model -> ( Model, Cmd Msg )
handleStartMergeUtxos scopeName scope rootUtxo model =
    if Dict.Any.size scope.treasuryUtxos > 1 then
        let
            mergeState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = MergeIdle
                }
        in
        ( { model | treasuryAction = MergeTreasuryUtxos mergeState }
        , Cmd.none
        )

    else
        ( { model | error = Just "Scope has insufficient UTXOs to merge" }
        , Cmd.none
        )


handleBuildMergeTransaction : Model -> List (Bytes CredentialHash) -> Posix -> ( Model, Cmd Msg )
handleBuildMergeTransaction model requiredSigners currentTime =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                _ =
                    Debug.log "currentTime" currentTime

                slotConfig =
                    case model.networkId of
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
                        |> Debug.log "validityRange"

                ( mergeTxIntents, mergeOtherInfo ) =
                    mergeUtxos model.networkId mergeState.rootUtxo mergeState.scope requiredSigners validityRange

                feeSource =
                    Cip30.walletChangeAddress wallet

                txResult =
                    TxIntent.finalizeAdvanced
                        { govState = TxIntent.emptyGovernanceState
                        , localStateUtxos = model.localStateUtxos
                        , coinSelectionAlgo = CoinSelection.largestFirst
                        , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource mergeTxIntents
                        , costModels = Uplc.conwayDefaultCostModels
                        }
                        (TxIntent.AutoFee { paymentSource = feeSource })
                        mergeOtherInfo
                        mergeTxIntents

                updatedMergeState =
                    { mergeState | status = MergeBuilding txResult }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }
            in
            ( updatedModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleSignMergeTransaction : Transaction -> Model -> ( Model, Cmd Msg )
handleSignMergeTransaction tx model =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                updatedMergeState =
                    { mergeState | status = MergeAwaitingSignature tx }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }

                signCmd =
                    toWallet <| Cip30.encodeRequest <| Cip30.signTx wallet { partialSign = True } tx
            in
            ( updatedModel, signCmd )

        _ ->
            ( model, Cmd.none )


handleSubmitMergeTransaction : Transaction -> Model -> ( Model, Cmd Msg )
handleSubmitMergeTransaction tx model =
    case ( model.treasuryAction, model.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                updatedMergeState =
                    { mergeState | status = MergeSubmitting tx }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }

                submitCmd =
                    toWallet <| Cip30.encodeRequest <| Cip30.submitTx wallet tx
            in
            ( updatedModel, submitCmd )

        _ ->
            ( model, Cmd.none )


{-| Merge all UTxOs from a given scope.
-}
mergeUtxos : NetworkId -> OutputReference -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> ( List TxIntent, List TxOtherInfo )
mergeUtxos networkId rootUtxo scope requiredSigners validityRange =
    let
        utxos =
            Dict.Any.toList scope.treasuryUtxos

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
                                , Witness.ByValue <| Script.cborWrappedBytes permissionsScript
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

        -- The address must have a stake cred (delegated to always abstain)
        scopeTreasuryAddress =
            Address.base networkId (Address.ScriptHash treasuryScriptHash) (Address.ScriptHash treasuryScriptHash)
    in
    Treasury.reorganize
        { treasuryScriptBytes = Script.cborWrappedBytes treasuryScript
        , registryOutputRef = Tuple.first scope.registryUtxo
        , additionalOutputRefs = [ rootUtxo ]
        , requiredSigners = requiredSigners
        , validityRange = validityRange
        , requiredWithdrawals = requiredWithdrawals
        , spentUtxos = utxos
        , receivers = \value -> [ TxIntent.SendTo scopeTreasuryAddress value ]
        }



-- Disburse


{-| Disburse funds from one UTxO in the given scope.

REMARK: you also need to add a `TxIntent.TxReferenceInput rootUtxoRef`

-}
disburse : NetworkId -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> OutputReference -> (Value -> List TxIntent) -> Value -> Result String ( List TxIntent, List TxOtherInfo )
disburse networkId scope requiredSigners validityRange utxoRef receivers value =
    case Dict.Any.get utxoRef scope.treasuryUtxos of
        Nothing ->
            Err <| "The selected UTxO isn’t in the known list of UTxOs for this scope: " ++ Debug.toString utxoRef

        Just spentOutput ->
            let
                spendConfig : SpendConfig
                spendConfig =
                    { treasuryScriptBytes = Script.cborWrappedBytes <| Tuple.second scope.sundaeTreasuryScript
                    , registryOutputRef = Tuple.first scope.registryUtxo
                    , requiredSigners = requiredSigners
                    , requiredWithdrawals = requiredWithdrawals
                    , spentInputRef = utxoRef
                    , spentOutput = spentOutput
                    , validityRange = validityRange
                    }

                requiredWithdrawals =
                    -- Withdrawal with the scope owner script
                    [ { stakeCredential =
                            { networkId = networkId
                            , stakeCredential = ScriptHash ownerScriptHash
                            }
                      , amount = N.zero
                      , scriptWitness =
                            Just <|
                                Witness.Plutus
                                    { script =
                                        ( Script.plutusVersion permissionsScript
                                        , Witness.ByValue <| Script.cborWrappedBytes permissionsScript
                                        )
                                    , redeemerData = \_ -> Debug.todo "Standard/Swap Owner Redeemer"
                                    , requiredSigners = requiredSigners
                                    }
                      }
                    ]

                ( ownerScriptHash, permissionsScript ) =
                    scope.permissionsScript

                overflowValue =
                    Value.subtract value spentOutput.amount
            in
            if overflowValue == Value.zero then
                Ok <| Treasury.disburse spendConfig receivers value

            else
                Err <| "Trying to disburse more than is available in this UTxO. Overflow value is: " ++ Debug.toString overflowValue



-- Tasks


handleTask : ConcurrentTask.Response String TaskCompleted -> Model -> ( Model, Cmd Msg )
handleTask response model =
    case ( response, model.page ) of
        ( ConcurrentTask.Error error, _ ) ->
            ( { model | error = Just error }, Cmd.none )

        ( ConcurrentTask.UnexpectedError error, _ ) ->
            ( { model | error = Just <| Debug.toString error }, Cmd.none )

        ( ConcurrentTask.Success taskCompleted, _ ) ->
            handleCompletedTask model taskCompleted


handleCompletedTask : Model -> TaskCompleted -> ( Model, Cmd Msg )
handleCompletedTask model taskCompleted =
    case taskCompleted of
        LoadedPragmaUtxo ( ref, output ) ->
            case model.treasuryManagement of
                TreasuryLoading loadingTreasury ->
                    case setPragmaUtxo model.scripts.sundaeTreasury ref output loadingTreasury of
                        Ok treasuryManagement ->
                            -- Also create a task to retrieve the scopes treasuries UTxOs
                            -- now that we have the applied sundae treasury scripts for each scope
                            let
                                allTreasuriesUtxosTask : ConcurrentTask String (BytesMap CredentialHash (Utxo.RefDict Output))
                                allTreasuriesUtxosTask =
                                    -- The list order is the same as the order of the scopes
                                    Api.retrieveUtxosWithPaymentCreds { db = model.db } model.networkId allTreasuriesScriptHashes

                                allTreasuriesScriptHashes =
                                    List.filterMap extractAppliedTreasuryScriptHash loadingScopes

                                loadingScopes =
                                    case treasuryManagement of
                                        TreasuryLoading { scopes, contingency } ->
                                            -- The list order is the same as the order of the scopes
                                            scopesToList scopes ++ [ contingency ]

                                        _ ->
                                            []

                                extractAppliedTreasuryScriptHash { sundaeTreasuryScriptApplied } =
                                    RemoteData.map (\( hash, _ ) -> hash) sundaeTreasuryScriptApplied
                                        |> RemoteData.toMaybe

                                ( updatedTaskPool, loadScopeUtxosCmd ) =
                                    allTreasuriesUtxosTask
                                        |> ConcurrentTask.map bytesMapToList
                                        |> ConcurrentTask.andThen listToScopes
                                        |> ConcurrentTask.map LoadedTreasuriesUtxos
                                        |> ConcurrentTask.attempt
                                            { pool = model.taskPool
                                            , send = sendTask
                                            , onComplete = OnTaskComplete
                                            }

                                bytesMapToList bytesMap =
                                    List.map (\hash -> Bytes.Map.get hash bytesMap |> Maybe.withDefault Utxo.emptyRefDict)
                                        allTreasuriesScriptHashes

                                listToScopes utxosInList =
                                    case utxosInList of
                                        [ ledgerUtxos, consensusUtxos, mercenariesUtxos, marketingUtxos, contingencyUtxos ] ->
                                            ConcurrentTask.succeed ( Scopes ledgerUtxos consensusUtxos mercenariesUtxos marketingUtxos, contingencyUtxos )

                                        _ ->
                                            let
                                                _ =
                                                    Debug.log "utxosInList" utxosInList
                                            in
                                            ConcurrentTask.fail "Something unexpected happened while trying to retrieve scopes treasuries UTxOs"
                            in
                            ( { model
                                | treasuryManagement = treasuryManagement
                                , taskPool = updatedTaskPool
                              }
                            , loadScopeUtxosCmd
                            )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadedRegistryUtxos ( registryUtxos, contingencyUtxo ) ->
            case model.treasuryManagement of
                TreasuryLoading ({ scopes, contingency } as loadingTreasury) ->
                    -- If all UTxOs have been loaded (registry and scope)
                    -- then we can convert into a LoadedTreasury
                    ( { model
                        | treasuryManagement =
                            TreasuryLoading
                                { loadingTreasury
                                    | scopes = setRegistryUtxos registryUtxos scopes
                                    , contingency = { contingency | registryUtxo = RemoteData.Success contingencyUtxo }
                                }
                      }
                        |> upgradeIfTreasuryLoadingFinished
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedTreasuriesUtxos ( treasuriesUtxos, contingencyUtxos ) ->
            case model.treasuryManagement of
                TreasuryLoading ({ scopes, contingency } as loadingTreasury) ->
                    -- If all UTxOs have been loaded (registry and scope)
                    -- then we can convert into a LoadedTreasury
                    ( { model
                        | treasuryManagement =
                            TreasuryLoading
                                { loadingTreasury
                                    | scopes = setTreasuryUtxos treasuriesUtxos scopes
                                    , contingency = { contingency | treasuryUtxos = RemoteData.Success contingencyUtxos }
                                }
                      }
                        |> upgradeIfTreasuryLoadingFinished
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


setRegistryUtxos : Scopes ( OutputReference, Output ) -> Scopes LoadingScope -> Scopes LoadingScope
setRegistryUtxos registryUtxos { ledger, consensus, mercenaries, marketing } =
    { ledger = { ledger | registryUtxo = RemoteData.Success registryUtxos.ledger }
    , consensus = { consensus | registryUtxo = RemoteData.Success registryUtxos.consensus }
    , mercenaries = { mercenaries | registryUtxo = RemoteData.Success registryUtxos.mercenaries }
    , marketing = { marketing | registryUtxo = RemoteData.Success registryUtxos.marketing }
    }


setTreasuryUtxos : Scopes (Utxo.RefDict Output) -> Scopes LoadingScope -> Scopes LoadingScope
setTreasuryUtxos treasuryUtxos { ledger, consensus, mercenaries, marketing } =
    { ledger = { ledger | treasuryUtxos = RemoteData.Success treasuryUtxos.ledger }
    , consensus = { consensus | treasuryUtxos = RemoteData.Success treasuryUtxos.consensus }
    , mercenaries = { mercenaries | treasuryUtxos = RemoteData.Success treasuryUtxos.mercenaries }
    , marketing = { marketing | treasuryUtxos = RemoteData.Success treasuryUtxos.marketing }
    }


{-| Extract the scopes owner config from the root pragma utxo.
Also apply the Sundae treasuy contracts with the now known paramaters.
-}
setPragmaUtxo : PlutusScript -> OutputReference -> Output -> LoadingTreasury -> Result String TreasuryManagement
setPragmaUtxo unappliedSundaeTreasuryScript ref output ({ contingency } as loading) =
    let
        scopes =
            case output.datumOption of
                Just (DatumValue { rawBytes }) ->
                    case Data.fromBytes rawBytes of
                        Nothing ->
                            Err "The PRAGMA UTxO datum does not contain valid Data."

                        Just data ->
                            updateLoadingScopesFromData unappliedSundaeTreasuryScript data loading.scopes

                _ ->
                    Err "The PRAGMA UTxO does not contain a valid datum"

        contingencyUpdated =
            { contingency | sundaeTreasuryScriptApplied = applySundaeTreasuryScript unappliedSundaeTreasuryScript loading.contingency }

        treasuryLoading okScopes =
            TreasuryLoading
                { loading
                    | rootUtxo = RemoteData.Success ( ref, output )
                    , scopes = okScopes
                    , contingency = contingencyUpdated
                }
    in
    Result.map treasuryLoading scopes


updateLoadingScopesFromData : PlutusScript -> Data -> Scopes LoadingScope -> Result String (Scopes LoadingScope)
updateLoadingScopesFromData unappliedSundaeTreasuryScript data { ledger, consensus, mercenaries, marketing } =
    case data of
        Data.Constr _ [ owner1, owner2, owner3, owner4 ] ->
            case ( ( MultisigScript.fromData owner1, MultisigScript.fromData owner2 ), ( MultisigScript.fromData owner3, MultisigScript.fromData owner4 ) ) of
                ( ( Just ms1, Just ms2 ), ( Just ms3, Just ms4 ) ) ->
                    let
                        updateOwner scope owner =
                            { scope
                                | owner = Just owner |> Debug.log "owner"
                                , sundaeTreasuryScriptApplied = applySundaeTreasuryScript unappliedSundaeTreasuryScript scope
                            }
                    in
                    Ok
                        { ledger = updateOwner ledger ms1
                        , consensus = updateOwner consensus ms2
                        , mercenaries = updateOwner mercenaries ms3
                        , marketing = updateOwner marketing ms4
                        }

                ( ( Nothing, _ ), _ ) ->
                    Err "The first scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( ( _, Nothing ), _ ) ->
                    Err "The second scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( Nothing, _ ) ) ->
                    Err "The third scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( _, Nothing ) ) ->
                    Err "The fourth scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

        _ ->
            Err "The PRAGMA UTxO does not contain exactly 4 owners"


applySundaeTreasuryScript : PlutusScript -> LoadingScope -> RemoteData String ( Bytes CredentialHash, PlutusScript )
applySundaeTreasuryScript unappliedScript scope =
    let
        multisig =
            MultisigScript.Script (Tuple.first scope.permissionsScriptApplied)

        treasuryConfig : Types.TreasuryConfiguration
        treasuryConfig =
            { registryToken = scope.registryNftPolicyId
            , permissions =
                { reorganize = multisig
                , sweep = multisig
                , fund = MultisigScript.AnyOf []
                , disburse = multisig
                }
            , expiration = scope.expiration
            , payoutUpperbound = N.zero
            }
    in
    Treasury.initializeScript treasuryConfig unappliedScript
        |> Result.Extra.unpack RemoteData.Failure
            (\applied -> RemoteData.Success ( Script.hash <| Script.Plutus applied, applied ))


upgradeIfTreasuryLoadingFinished : Model -> Model
upgradeIfTreasuryLoadingFinished model =
    case model.treasuryManagement of
        TreasuryLoading { rootUtxo, pragmaScriptHash, registriesSeedUtxo, expiration, scopes, contingency } ->
            case ( rootUtxo, upgradeScopesIfLoadingFinished scopes, upgradeScope contingency ) of
                ( RemoteData.Success ( ref, output ), Just loadedScopes, Just contingencyScope ) ->
                    -- Upgrade the treasury management
                    -- AND the local state utxos
                    { model
                        | treasuryManagement =
                            TreasuryFullyLoaded
                                { rootUtxo = ( ref, output )
                                , scopesScriptHash = pragmaScriptHash
                                , registriesSeedUtxoRef = registriesSeedUtxo
                                , expiration = expiration
                                , scopes = loadedScopes
                                , contingency = contingencyScope
                                }
                        , localStateUtxos = addLoadedUtxos ( ref, output ) loadedScopes model.localStateUtxos
                    }

                _ ->
                    model

        _ ->
            model


upgradeScopesIfLoadingFinished : Scopes LoadingScope -> Maybe (Scopes Scope)
upgradeScopesIfLoadingFinished { ledger, consensus, mercenaries, marketing } =
    Maybe.map4 Scopes
        (upgradeScope ledger)
        (upgradeScope consensus)
        (upgradeScope mercenaries)
        (upgradeScope marketing)


upgradeScope : LoadingScope -> Maybe Scope
upgradeScope scope =
    case ( scope.sundaeTreasuryScriptApplied, scope.registryUtxo, scope.treasuryUtxos ) of
        ( RemoteData.Success treasuryScriptApplied, RemoteData.Success registryUtxo, RemoteData.Success treasuryUtxos ) ->
            Just
                { owner = scope.owner |> Maybe.withDefault (MultisigScript.AnyOf [])
                , permissionsScript = scope.permissionsScriptApplied
                , sundaeTreasuryScript = treasuryScriptApplied
                , registryUtxo = registryUtxo
                , treasuryUtxos = treasuryUtxos
                }

        _ ->
            Nothing


addLoadedUtxos : ( OutputReference, Output ) -> Scopes Scope -> Utxo.RefDict Output -> Utxo.RefDict Output
addLoadedUtxos ( rootRef, rootOutput ) { ledger, consensus, mercenaries, marketing } localState =
    let
        addScopeUtxos scope accum =
            Dict.Any.union scope.treasuryUtxos accum
                |> Dict.Any.insert (Tuple.first scope.registryUtxo) (Tuple.second scope.registryUtxo)
    in
    addScopeUtxos ledger localState
        |> addScopeUtxos consensus
        |> addScopeUtxos mercenaries
        |> addScopeUtxos marketing
        |> Dict.Any.insert rootRef rootOutput



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            viewHome model

        SignTxPage pageModel ->
            let
                viewContext =
                    { wrapMsg = SignTxMsg
                    , toCallbackPage =
                        \subject ->
                            case subject of
                                SignTx.Unknown ->
                                    GoToHomePage

                                SignTx.TreasurySetup ->
                                    GoToHomePage
                    , wallet = model.connectedWallet
                    , networkId = model.networkId
                    }
            in
            SignTx.view viewContext pageModel


viewHome : Model -> Html Msg
viewHome model =
    div []
        [ viewError model.error
        , viewWalletSection model
        , viewLocalStateUtxosSection model.localStateUtxos
        , case model.treasuryAction of
            NoTreasuryAction ->
                viewTreasurySection model.treasuryLoadingParams model.treasuryManagement

            MergeTreasuryUtxos mergeState ->
                viewMergeUtxosAction model.connectedWallet mergeState
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            Html.pre [] [ text <| "ERROR:\n" ++ error ]



-- Wallet


viewWalletSection : Model -> Html Msg
viewWalletSection { discoveredWallets, connectedWallet } =
    case connectedWallet of
        Nothing ->
            div []
                [ div [] [ text "CIP-30 wallets detected:" ]
                , viewAvailableWallets discoveredWallets
                ]

        Just wallet ->
            viewConnectedWallet wallet


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        connectButton { id } =
            Html.button [ onClick (ConnectButtonClicked { id = id }) ] [ text "connect" ]

        walletRow w =
            div [] [ walletIcon w, text <| walletDescription w ++ " ", connectButton w ]
    in
    div [] (List.map walletRow wallets)


viewConnectedWallet : Cip30.Wallet -> Html Msg
viewConnectedWallet wallet =
    div []
        [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
        , div [] [ text <| "Address: " ++ (Address.toBech32 <| Cip30.walletChangeAddress wallet) ]
        , Html.button [ onClick DisconnectWallet ] [ text "Disconnect" ]
        ]



-- Local state UTxOs


viewLocalStateUtxosSection : Utxo.RefDict Output -> Html msg
viewLocalStateUtxosSection utxos =
    Html.p [] [ text <| "Local state UTxOs size: " ++ String.fromInt (Dict.Any.size utxos) ]



-- Treasury Section


viewTreasurySection : TreasuryLoadingParams -> TreasuryManagement -> Html Msg
viewTreasurySection params treasuryManagement =
    case treasuryManagement of
        TreasuryUnspecified ->
            div []
                [ Html.p [] [ Html.text "Treasury unspecified yet" ]
                , Html.button [ onClick StartTreasurySetup ]
                    [ text "Setup a new treasury" ]
                , text " or "
                , Html.button [ onClick StartTreasuryLoading ]
                    [ text "Load pre-initialized treasury" ]
                , Html.p [] [ text "Override treasury loading parameters:" ]
                , Html.p []
                    [ Html.label [] [ text "Pragma Scopes script hash: " ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value params.pragmaScriptHash
                        , HE.onInput (TreasuryLoadingParamsMsg << UpdatePragmaScriptHash)
                        ]
                        []
                    ]
                , Html.p []
                    [ Html.label [] [ text "Registries Seed UTxO - Tx ID: " ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value params.registriesSeedUtxo.transactionId
                        , HE.onInput (TreasuryLoadingParamsMsg << UpdateRegistriesSeedTransactionId)
                        ]
                        []
                    ]
                , Html.p []
                    [ Html.label [] [ text "Registries Seed UTxO - Output Index: " ]
                    , Html.input
                        [ HA.type_ "number"
                        , HA.value <| String.fromInt params.registriesSeedUtxo.outputIndex
                        , HE.onInput (TreasuryLoadingParamsMsg << UpdateRegistriesSeedOutputIndex)
                        ]
                        []
                    ]
                , Html.p []
                    [ Html.label [] [ text "Expiration date (Posix): " ]
                    , Html.input
                        [ HA.type_ "number"
                        , HA.value <| String.fromInt params.treasuryConfigExpiration
                        , HE.onInput (TreasuryLoadingParamsMsg << UpdateExpiration)
                        ]
                        []
                    , text <| " (" ++ displayPosixDate (Time.millisToPosix params.treasuryConfigExpiration) ++ ")"
                    ]
                ]

        TreasurySetupForm form ->
            viewTreasurySetupForm form

        TreasurySetupTxs state ->
            viewSetupTxsState state

        TreasuryLoading { rootUtxo, pragmaScriptHash, registriesSeedUtxo, treasuryConfigExpiration, scopes, contingency } ->
            div []
                [ Html.p [] [ text "Loading treasury ... ", spinner ]
                , viewLoadingRootUtxo rootUtxo
                , viewPragmaScopesScriptHash pragmaScriptHash
                , viewRegistriesSeedUtxo registriesSeedUtxo
                , viewExpirationDate <| N.toInt treasuryConfigExpiration
                , viewLoadingScope "ledger" scopes.ledger
                , viewLoadingScope "consensus" scopes.consensus
                , viewLoadingScope "mercenaries" scopes.mercenaries
                , viewLoadingScope "marketing" scopes.marketing
                , viewLoadingScope "contingency" contingency
                ]

        TreasuryFullyLoaded { rootUtxo, scopesScriptHash, registriesSeedUtxoRef, expiration, scopes, contingency } ->
            let
                rootUtxoRef =
                    Tuple.first rootUtxo
            in
            div []
                [ Html.p [] [ text "Treasury fully loaded" ]
                , viewReload scopesScriptHash registriesSeedUtxoRef expiration
                , viewRootUtxo rootUtxo
                , viewScope rootUtxoRef "ledger" scopes.ledger
                , viewScope rootUtxoRef "consensus" scopes.consensus
                , viewScope rootUtxoRef "mercenaries" scopes.mercenaries
                , viewScope rootUtxoRef "marketing" scopes.marketing
                , viewScope rootUtxoRef "contingency" contingency
                ]


viewReload : Bytes CredentialHash -> OutputReference -> Posix -> Html msg
viewReload scopesScriptHash registriesSeedUtxoRef expiration =
    Html.div [ HA.style "padding" "16px", HA.style "box-shadow" "0 0 16px rgba(0, 0, 0, 0.2)" ]
        [ Html.p [] [ Html.strong [] [ text "KEEP this to be able to reload the Treasury:" ] ]
        , viewPragmaScopesScriptHash scopesScriptHash
        , viewRegistriesSeedUtxo registriesSeedUtxoRef
        , viewExpirationDate <| Time.posixToMillis expiration
        ]


viewPragmaScopesScriptHash : Bytes CredentialHash -> Html msg
viewPragmaScopesScriptHash scriptHash =
    Html.p [] [ text <| "(PRAGMA) Scopes script hash: " ++ Bytes.toHex scriptHash ]


viewRegistriesSeedUtxo : OutputReference -> Html msg
viewRegistriesSeedUtxo { transactionId, outputIndex } =
    Html.p [] [ text <| "Registries Seed UTXO: " ++ Bytes.toHex transactionId ++ " #" ++ String.fromInt outputIndex ]


viewExpirationDate : Int -> Html msg
viewExpirationDate posixDate =
    Html.p [] [ text <| "Expiration date (Posix): " ++ String.fromInt posixDate ++ " (" ++ displayPosixDate (Time.millisToPosix posixDate) ++ ")" ]


displayPosixDate : Posix -> String
displayPosixDate posix =
    let
        year =
            Time.toYear Time.utc posix
                |> String.fromInt

        month =
            Time.toMonth Time.utc posix
                |> Debug.toString

        day =
            Time.toDay Time.utc posix
                |> String.fromInt
    in
    "UTC: " ++ year ++ " / " ++ month ++ " / " ++ day


viewTreasurySetupForm : SetupForm -> Html Msg
viewTreasurySetupForm form =
    case form.validation of
        Nothing ->
            viewTreasurySetupFormHelper form Nothing

        Just (Err errors) ->
            viewTreasurySetupFormHelper form (Just errors)

        Just (Ok { expiration, scopeOwners }) ->
            viewScopeOwnersSetup expiration scopeOwners


viewScopeOwnersSetup : Int -> Scopes MultisigScript -> Html Msg
viewScopeOwnersSetup expiration scopeOwners =
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
            [ Html.button [ onClick <| UpdateSetupForm TryBuildSetupTxs ]
                [ text "Build the Txs" ]
            ]
        ]


viewTreasurySetupFormHelper : SetupForm -> Maybe String -> Html Msg
viewTreasurySetupFormHelper form errors =
    div []
        [ Html.h2 [] [ text "Setup New Treasury" ]
        , Html.p []
            [ Html.label [] [ text "Expiration date (Posix): " ]
            , Html.input
                [ HA.type_ "number"
                , HA.value <| String.fromInt form.expiration
                , HE.onInput (UpdateSetupForm << ExpirationField)
                ]
                []
            , text <| " (" ++ displayPosixDate (Time.millisToPosix form.expiration) ++ ")"
            ]
        , div []
            [ Html.h3 [] [ text "Scope Owners" ]
            , Html.p [] [ text "Enter the aicone multisigs for each scope owner (for now just enter a public key hash):" ]
            ]
        , div []
            [ viewFormField "Ledger" form.ledgerOwner LedgerOwnerField
            , viewFormField "Consensus" form.consensusOwner ConsensusOwnerField
            , viewFormField "Mercenaries" form.mercenariesOwner MercenariesOwnerField
            , viewFormField "Marketing" form.marketingOwner MarketingOwnerField
            ]
        , Html.p []
            [ Html.button [ onClick (UpdateSetupForm ValidateSetupForm) ]
                [ text "Validate scope owners & prepare Txs" ]
            ]
        , viewFormErrors errors
        ]


viewFormField : String -> String -> (String -> SetupFormMsg) -> Html Msg
viewFormField label value field =
    div []
        [ Html.label [] [ text <| label ++ ": " ]
        , Html.input
            [ HA.type_ "text"
            , HA.placeholder "Enter public key hash"
            , HE.onInput (UpdateSetupForm << field)
            , HA.value value
            ]
            []
        ]


viewFormErrors : Maybe String -> Html Msg
viewFormErrors maybeErrors =
    case maybeErrors of
        Nothing ->
            text ""

        Just errors ->
            Html.pre [ HA.style "color" "red" ] [ text errors ]


viewSetupTxsState : SetupTxsState -> Html Msg
viewSetupTxsState { txs, treasury, tracking } =
    div []
        [ Html.h2 [] [ text "Treasury State after Initialization" ]
        , viewRootUtxo treasury.rootUtxo
        , viewScopeSetup "ledger" treasury.scopes.ledger
        , viewScopeSetup "consensus" treasury.scopes.consensus
        , viewScopeSetup "mercenaries" treasury.scopes.mercenaries
        , viewScopeSetup "marketing" treasury.scopes.marketing
        , viewScopeSetup "contingency" treasury.contingency
        , Html.h2 [] [ text "Txs to submit for Treasury Initialization" ]
        , Html.h3 [] [ text "Scope Owners Definition" ]
        , viewTxStatus tracking.scopes txs.scopes
        , Html.h3 [] [ text "Permissions Stake Registration" ]
        , viewTxStatus tracking.permissions txs.permissions
        , Html.h3 [] [ text "Registries Initializations" ]
        , viewTxStatus tracking.registries txs.registries
        ]


viewTxStatus : TxState -> SignTx.Prep -> Html Msg
viewTxStatus txState ({ txId, tx, expectedSignatures, signerDescriptions } as signingPrep) =
    let
        displaySigner signer =
            case Bytes.Map.get signer signerDescriptions of
                Nothing ->
                    Html.li [] [ text <| "Signer: " ++ Bytes.toHex signer ]

                Just description ->
                    Html.li [] [ text <| description ++ ": " ++ Bytes.toHex signer ]

        actions =
            case txState of
                TxNotSubmittedYet ->
                    Html.button
                        [ onClick <| SignTxButtonClicked SignTx.TreasurySetup signingPrep ]
                        [ text "Sign on signing page" ]

                TxSubmitting ->
                    div [] [ text "Tx is being submitted ... ", spinner ]

                TxSubmitted ->
                    text "Tx has been submitted!"
    in
    div []
        [ Html.p [] [ text <| "Tx state: " ++ Debug.toString txState ]
        , Html.p []
            [ text <| "Expected signers:"
            , Html.ul [] <| List.map displaySigner expectedSignatures
            ]
        , Html.p [] [ actions ]
        , Html.p [] [ text <| "Tx details:" ]
        , Html.p [] [ text <| "Tx ID: " ++ Bytes.toHex txId ]
        , Html.pre [] [ text <| prettyTx tx ]
        ]


viewLoadingRootUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewLoadingRootUtxo rootUtxo =
    case rootUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text "(PRAGMA) root scopes UTxO not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text "(PRAGMA) root scopes UTxO loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "(PRAGMA) root scopes UTxO failed to load: " ++ error ]

        RemoteData.Success utxo ->
            viewRootUtxo utxo


viewRootUtxo : ( OutputReference, Output ) -> Html msg
viewRootUtxo ( ref, _ ) =
    Html.p [] [ text <| "(PRAGMA) root scopes UTxO: " ++ Utxo.refAsString ref ]


viewLoadingScope : String -> LoadingScope -> Html msg
viewLoadingScope scopeName { owner, permissionsScriptApplied, sundaeTreasuryScriptApplied, registryNftPolicyId, registryUtxo, treasuryUtxos } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewMaybeOwner owner
        , viewPermissionsScript permissionsScriptApplied
        , viewLoadingTreasuryScript sundaeTreasuryScriptApplied
        , viewRegistryNftPolicyId registryNftPolicyId
        , viewLoadingRegistryUtxo registryUtxo
        , viewLoadingTreasuryUtxos treasuryUtxos
        ]


viewScopeSetup : String -> Scope -> Html Msg
viewScopeSetup scopeName { owner, permissionsScript, sundaeTreasuryScript, registryUtxo } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewTreasuryScript sundaeTreasuryScript
        , viewRegistryUtxo registryUtxo
        ]


viewScope : OutputReference -> String -> Scope -> Html Msg
viewScope rootUtxo scopeName ({ owner, permissionsScript, sundaeTreasuryScript, registryUtxo, treasuryUtxos } as scope) =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewTreasuryScript sundaeTreasuryScript
        , viewRegistryUtxo registryUtxo
        , viewTreasuryUtxos scopeName scope rootUtxo treasuryUtxos
        ]


viewMaybeOwner : Maybe MultisigScript -> Html msg
viewMaybeOwner maybeOwner =
    case maybeOwner of
        Nothing ->
            Html.p [] [ text <| "Owner: loading ... ", spinner ]

        Just owner ->
            viewOwner owner


viewOwner : MultisigScript -> Html msg
viewOwner owner =
    Html.p [] [ text <| "Owner: " ++ Debug.toString owner ]


viewPermissionsScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewPermissionsScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied permissions script hash: " ++ Bytes.toHex hash ]


viewLoadingTreasuryScript : RemoteData String ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewLoadingTreasuryScript remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Sundae treasury script not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Sundae treasury script loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Sundae treasury script failed to load: " ++ error ]

        RemoteData.Success script ->
            viewTreasuryScript script


viewTreasuryScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewTreasuryScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied Sundae treasury script hash: " ++ Bytes.toHex hash ]


viewRegistryNftPolicyId : Bytes PolicyId -> Html msg
viewRegistryNftPolicyId policyId =
    Html.p [] [ text <| "Registry trap policy ID: " ++ Bytes.toHex policyId ]


viewLoadingRegistryUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewLoadingRegistryUtxo registryUtxo =
    case registryUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Registry UTxO not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Registry UTxO loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Registry UTxO failed to load: " ++ error ]

        RemoteData.Success utxo ->
            viewRegistryUtxo utxo


viewRegistryUtxo : ( OutputReference, Output ) -> Html msg
viewRegistryUtxo ( ref, _ ) =
    Html.p [] [ text <| "Registry UTxO: " ++ Utxo.refAsString ref ]


viewLoadingTreasuryUtxos : RemoteData String (Utxo.RefDict Output) -> Html msg
viewLoadingTreasuryUtxos loadingUtxos =
    case loadingUtxos of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Treasury UTxOs not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Treasury UTxOs loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Treasury UTxOs failed to load: " ++ error ]

        RemoteData.Success utxos ->
            Html.p [] [ text <| "Treasury UTxOs loaded. UTxO count = " ++ String.fromInt (Dict.Any.size utxos) ]


viewTreasuryUtxos : String -> Scope -> OutputReference -> Utxo.RefDict Output -> Html Msg
viewTreasuryUtxos scopeName scope rootUtxo utxos =
    let
        mergeButton =
            if Dict.Any.size utxos > 1 then
                Html.button [ onClick (StartMergeUtxos scopeName scope rootUtxo) ] [ text "Merge UTxOs" ]

            else
                text ""
    in
    div []
        [ Html.p [] [ text <| "Treasury UTxOs count: " ++ String.fromInt (Dict.Any.size utxos) ]
        , Html.p [] [ text <| "TODO: add buttons for possible actions with those UTxOs" ]
        , Html.map TreasuryMergingMsg mergeButton
        , Html.ul [] <|
            List.map viewDetailedUtxo (Dict.Any.toList utxos)
        ]


viewDetailedUtxo : ( OutputReference, Output ) -> Html msg
viewDetailedUtxo ( ref, output ) =
    Html.li []
        [ div [] [ text <| "UTxO: " ++ Utxo.refAsString ref ]
        , div [] [ text <| "Address: " ++ Address.toBech32 output.address ]
        , div [] [ text <| "Value: (₳ amounts are in Lovelaces)" ]
        , Html.pre [] [ text <| String.join "\n" <| Value.toMultilineString output.amount ]
        ]


spinner : Html msg
spinner =
    Html.span [ HA.class "loader" ] []



-- Merge UTxOs


viewMergeUtxosAction : Maybe Cip30.Wallet -> MergeTreasuryUtxosState -> Html Msg
viewMergeUtxosAction maybeWallet mergeState =
    Html.map TreasuryMergingMsg <|
        div []
            [ Html.h3 [] [ text ("Merge UTXOs - " ++ mergeState.scopeName ++ " Scope") ]
            , div [] [ text ("UTXOs to merge: " ++ String.fromInt (Dict.Any.size mergeState.scope.treasuryUtxos)) ]
            , Html.ul [] <|
                List.map viewDetailedUtxo (Dict.Any.toList mergeState.scope.treasuryUtxos)
            , case mergeState.status of
                MergeIdle ->
                    let
                        -- FIXME: temporary, something more robust should be done,
                        -- and not inside the view, but this works as a MVP.
                        requiredSigners =
                            MultisigScript.extractRequiredSigners mergeState.scope.owner
                    in
                    case maybeWallet of
                        Nothing ->
                            div []
                                [ Html.p [] [ text "Please connect your wallet first, to pay the Tx fees." ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                        Just _ ->
                            div []
                                [ Html.p [] [ text "Ready to build merge transaction. This will combine all UTXOs in this scope into a single UTXO." ]
                                , Html.button [ onClick (BuildMergeTransaction requiredSigners) ] [ text "Build Transaction" ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                MergeBuilding txFinalizedResult ->
                    case txFinalizedResult of
                        Err error ->
                            div []
                                [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                                , Html.pre [] [ text <| TxIntent.errorToString error ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                        Ok { tx, expectedSignatures } ->
                            div []
                                [ Html.p [] [ text "Merge Tx to be signed:" ]
                                , Html.pre [] [ text <| prettyTx tx ]
                                , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                                ]

                MergeAwaitingSignature transaction ->
                    div []
                        [ Html.p [] [ text "Transaction built successfully. Please sign the transaction." ]
                        , Html.button [ onClick (SignMergeTransaction transaction) ] [ text "Sign Transaction" ]
                        , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                        ]

                MergeSubmitting transaction ->
                    div [] [ text " Submitting transaction...", spinner ]
            ]
