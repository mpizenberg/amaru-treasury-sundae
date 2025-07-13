module TreasuryManagement exposing (..)

import Api
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Script as Script exposing (NativeScript(..), PlutusScript, PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxFinalized, TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Cmd.Extra
import ConcurrentTask exposing (ConcurrentTask)
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N exposing (Natural)
import Page.SignTx as SignTx
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData)
import Result.Extra
import Route
import Storage
import Task
import Time exposing (Posix)
import Treasury exposing (SpendConfig)
import TreasuryManagement.Scope exposing (Scope)
import TreasuryManagement.Scopes as Scopes exposing (Scopes)
import TreasuryManagement.Setup exposing (Scripts, SetupTxs)
import TreasuryManagement.SetupForm as SetupForm exposing (SetupForm)
import Types
import Utils exposing (displayPosixDate, viewError)


type alias Model =
    { treasuryLoadingParamsForm : TreasuryLoadingParamsForm
    , treasuryManagement : TreasuryManagement
    , treasuryAction : TreasuryAction
    , scripts : Scripts
    , error : Maybe String
    }


init : Scripts -> TreasuryLoadingParamsForm -> Model
init scripts treasuryLoadingParamsForm =
    { treasuryLoadingParamsForm = treasuryLoadingParamsForm
    , treasuryManagement = TreasuryUnspecified
    , treasuryAction = NoTreasuryAction
    , scripts = scripts
    , error = Nothing
    }


setLoadingParamsForm : TreasuryLoadingParamsForm -> Model -> Model
setLoadingParamsForm treasuryLoadingParamsForm model =
    { model | treasuryLoadingParamsForm = treasuryLoadingParamsForm }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasurySetupForm SetupForm
    | TreasurySetupTxs SetupTxsState
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


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


treasuryLoadingScopes : LoadingTreasury -> List LoadingScope
treasuryLoadingScopes { scopes, contingency } =
    -- The list order is the same as the order of the scopes
    Scopes.toList scopes ++ [ contingency ]


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


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , loadingParams : LoadingParams
    , scopes : Scopes Scope
    , contingency : Scope
    }


upgradeIfTreasuryLoadingFinished : Utxo.RefDict Output -> TreasuryManagement -> ( TreasuryManagement, Utxo.RefDict Output )
upgradeIfTreasuryLoadingFinished localStateUtxos treasuryManagement =
    case treasuryManagement of
        TreasuryLoading { rootUtxo, loadingParams, scopes, contingency } ->
            case ( rootUtxo, upgradeScopesIfLoadingFinished scopes, upgradeScope contingency ) of
                ( RemoteData.Success ( ref, output ), Just loadedScopes, Just contingencyScope ) ->
                    -- Upgrade the treasury management
                    -- AND the local state utxos
                    ( TreasuryFullyLoaded
                        { rootUtxo = ( ref, output )
                        , loadingParams = loadingParams
                        , scopes = loadedScopes
                        , contingency = contingencyScope
                        }
                    , addLoadedUtxos ( ref, output ) loadedScopes contingencyScope localStateUtxos
                    )

                _ ->
                    ( treasuryManagement, localStateUtxos )

        _ ->
            ( treasuryManagement, localStateUtxos )


upgradeScopesIfLoadingFinished : Scopes LoadingScope -> Maybe (Scopes Scope)
upgradeScopesIfLoadingFinished { ledger, consensus, mercenaries, marketing } =
    Maybe.map4 Scopes
        (upgradeScope ledger)
        (upgradeScope consensus)
        (upgradeScope mercenaries)
        (upgradeScope marketing)


upgradeScope : LoadingScope -> Maybe Scope
upgradeScope scope =
    -- TODO: check for treasury ref scripts
    case ( scope.permissionsScriptPublished, ( scope.sundaeTreasuryScriptApplied, scope.sundaeTreasuryScriptPublished, scope.treasuryUtxos ), scope.registryUtxo ) of
        ( RemoteData.Success maybePermissionsScriptUtxo, ( RemoteData.Success treasuryScriptApplied, RemoteData.Success maybeTreasuryScriptUtxo, RemoteData.Success treasuryUtxos ), RemoteData.Success registryUtxo ) ->
            Just
                { owner = scope.owner |> Maybe.withDefault (MultisigScript.AnyOf [])
                , permissionsScript = scope.permissionsScriptApplied
                , permissionsScriptRef = maybePermissionsScriptUtxo
                , sundaeTreasuryScript = treasuryScriptApplied
                , sundaeTreasuryScriptRef = maybeTreasuryScriptUtxo
                , registryUtxo = registryUtxo
                , treasuryUtxos = treasuryUtxos
                }

        _ ->
            Nothing


addLoadedUtxos : ( OutputReference, Output ) -> Scopes Scope -> Scope -> Utxo.RefDict Output -> Utxo.RefDict Output
addLoadedUtxos ( rootRef, rootOutput ) { ledger, consensus, mercenaries, marketing } contingency localState =
    let
        addScopeUtxos scope accum =
            Dict.Any.union scope.treasuryUtxos accum
                |> Dict.Any.insert (Tuple.first scope.registryUtxo) (Tuple.second scope.registryUtxo)
                |> (scope.sundaeTreasuryScriptRef
                        |> Maybe.map (\( ref, output ) -> Dict.Any.insert ref output)
                        |> Maybe.withDefault identity
                   )
    in
    addScopeUtxos ledger localState
        |> addScopeUtxos consensus
        |> addScopeUtxos mercenaries
        |> addScopeUtxos marketing
        |> addScopeUtxos contingency
        |> Dict.Any.insert rootRef rootOutput


doubleCheckTreasuryScriptsHashes : TreasuryManagement -> TreasuryLoadingParamsForm -> ( TreasuryManagement, TreasuryLoadingParamsForm )
doubleCheckTreasuryScriptsHashes treasuryManagement treasuryLoadingParamsForm =
    -- Make sure the scripts hashes obtained from applying the scripts
    -- match the ones obtained from the registry datums
    case treasuryManagement of
        TreasuryFullyLoaded loadedTreasury ->
            (Scopes.toList loadedTreasury.scopes ++ [ loadedTreasury.contingency ])
                |> Result.Extra.combineMap doublecheckTreasuryScriptHash
                |> Result.Extra.unpack
                    (\error ->
                        ( TreasuryUnspecified, { treasuryLoadingParamsForm | error = Just error } )
                    )
                    (\_ -> ( treasuryManagement, treasuryLoadingParamsForm ))

        _ ->
            ( treasuryManagement, treasuryLoadingParamsForm )


doublecheckTreasuryScriptHash : Scope -> Result String ()
doublecheckTreasuryScriptHash { sundaeTreasuryScript, registryUtxo } =
    let
        ( computedScriptHash, _ ) =
            sundaeTreasuryScript

        ( registryRef, registryOutput ) =
            registryUtxo

        -- Decode the output datum into Types.ScriptHashRegistry
        treasuryHashDecodedFromRegistry =
            case registryOutput.datumOption of
                Nothing ->
                    Err <| "No datum found in the supposed registry UTxO: " ++ Utxo.refAsString registryRef

                Just (DatumHash _) ->
                    Err <| "The registry UTxO datum is a datum hash instead of a datum value"

                Just (DatumValue { rawBytes }) ->
                    Data.fromBytes rawBytes
                        |> Maybe.andThen Types.registryFromData
                        |> Result.fromMaybe ("The registry UTxO datum does not contain a valid registry. UTxO ref: " ++ Utxo.refAsString registryRef)
    in
    treasuryHashDecodedFromRegistry
        |> Result.andThen
            (\{ treasury } ->
                if treasury == ScriptHash computedScriptHash then
                    Ok ()

                else
                    Err <| "Treasury script hash mismatch between the one in the onchain registry UTxO, and the one re-computed from the treasury script blueprint. Make sure you are compiling the aiken contracts with the correct debug options."
            )


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


treasuryLoadingParamsFormDecoder : JD.Decoder TreasuryLoadingParamsForm
treasuryLoadingParamsFormDecoder =
    JD.map4 TreasuryLoadingParamsForm
        (JD.field "pragmaScriptHash" JD.string)
        (JD.field "registriesSeedUtxo"
            (JD.map2 (\a b -> { transactionId = a, outputIndex = b })
                (JD.field "transactionId" JD.string)
                (JD.field "outputIndex" JD.int)
            )
        )
        (JD.field "treasuryConfigExpiration" JD.int)
        (JD.succeed Nothing)


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



-- UPDATE ############################################################


type Msg
    = StartTreasurySetup
    | StartTreasurySetupWithCurrentTime Posix
    | UpdateSetupForm SetupForm.Msg
    | TryBuildSetupTxs
    | TreasuryLoadingParamsMsg TreasuryLoadingParamsMsg
    | StartTreasuryLoading
    | RefreshTreasuryUtxos
    | TreasuryMergingMsg TreasuryMergingMsg
    | TreasuryDisburseMsg TreasuryDisburseMsg
    | PublishScript (Bytes CredentialHash) PlutusScript


type TreasuryLoadingParamsMsg
    = UpdatePragmaScriptHash String
    | UpdateRegistriesSeedTransactionId String
    | UpdateRegistriesSeedOutputIndex String
    | UpdateExpiration String


type TreasuryMergingMsg
    = StartMergeUtxos String Scope OutputReference
    | BuildMergeTransaction (List (Bytes CredentialHash))
    | BuildMergeTransactionWithTime (List (Bytes CredentialHash)) Posix
    | CancelMergeAction


type TreasuryDisburseMsg
    = StartDisburse StartDisburseInfo
    | CheckOwner String Bool
    | AddRecipient
    | RemoveRecipient Int
    | SetRecipientAddress Int String
    | SetRecipientValue Int String
    | BuildDisburseTransaction
    | BuildDisburseTransactionWithTime Posix
    | CancelDisburseAction


type alias StartDisburseInfo =
    { scopeName : String
    , scope : Scope
    , allOwners : List ( String, MultisigScript )
    , rootUtxo : OutputReference
    , spendingUtxo : ( OutputReference, Output )
    }


type TaskCompleted
    = LoadingParamsSaved
    | LoadedPragmaUtxo ( OutputReference, Output )
    | LoadedRegistryUtxos ( Scopes ( OutputReference, Output ), ( OutputReference, Output ) )
    | LoadedTreasuriesUtxos ( Scopes (Utxo.RefDict Output), Utxo.RefDict Output )
    | LoadedTreasuryRefScript (Bytes CredentialHash) (Maybe ( OutputReference, Output ))
    | LoadedPermissionsRefScript (Bytes CredentialHash) (Maybe ( OutputReference, Output ))


type alias UpdateContext a msg =
    { a
        | toMsg : Msg -> msg
        , routingConfig : Route.Config msg
        , db : JD.Value
        , connectedWallet : Maybe Cip30.Wallet
        , localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
    }


type alias OutMsg =
    { updatedLocalState : Utxo.RefDict Output
    , runTasks : List (ConcurrentTask String TaskCompleted)
    }


update : UpdateContext a msg -> Msg -> Model -> ( Model, Cmd msg, OutMsg )
update ctx msg model =
    let
        noOutMsg =
            { updatedLocalState = ctx.localStateUtxos
            , runTasks = []
            }
    in
    case msg of
        StartTreasurySetup ->
            ( model
            , Task.perform (ctx.toMsg << StartTreasurySetupWithCurrentTime) Time.now
            , noOutMsg
            )

        StartTreasurySetupWithCurrentTime currentTime ->
            ( { model | treasuryManagement = TreasurySetupForm <| SetupForm.init currentTime }, Cmd.none, noOutMsg )

        UpdateSetupForm subMsg ->
            ( handleTreasurySetupFormUpdate subMsg model, Cmd.none, noOutMsg )

        TryBuildSetupTxs ->
            case model.treasuryManagement of
                TreasurySetupForm form ->
                    ( { model | treasuryManagement = handleBuildSetupTxs ctx model.scripts form }, Cmd.none, noOutMsg )

                _ ->
                    ( model, Cmd.none, noOutMsg )

        TreasuryLoadingParamsMsg paramsMsg ->
            ( handleTreasuryLoadingParamsMsg paramsMsg model, Cmd.none, noOutMsg )

        StartTreasuryLoading ->
            startTreasuryLoading ctx model

        RefreshTreasuryUtxos ->
            refreshTreasuryUtxos ctx model

        TreasuryMergingMsg submsg ->
            handleTreasuryMergingMsg ctx submsg model

        TreasuryDisburseMsg submsg ->
            handleTreasuryDisburseMsg ctx submsg model

        PublishScript hash script ->
            createPublishScriptTx ctx hash script model


handleTreasurySetupFormUpdate : SetupForm.Msg -> Model -> Model
handleTreasurySetupFormUpdate msg model =
    case model.treasuryManagement of
        TreasurySetupForm form ->
            { model | treasuryManagement = TreasurySetupForm <| SetupForm.update msg form }

        _ ->
            model


handleBuildSetupTxs : UpdateContext a msg -> Scripts -> SetupForm -> TreasuryManagement
handleBuildSetupTxs ctx scripts form =
    case form.validation of
        Just (Ok scopeOwners) ->
            case ctx.connectedWallet of
                Nothing ->
                    TreasurySetupForm { form | validation = Just <| Err "Please connect wallet first" }

                Just wallet ->
                    case TreasuryManagement.Setup.setupAmaruTreasury ctx.localStateUtxos scripts ctx.networkId wallet scopeOwners of
                        Err error ->
                            TreasurySetupForm { form | validation = Just <| Err error }

                        Ok ( txs, treasury ) ->
                            TreasurySetupTxs <| initSetupTxsState txs treasury

        _ ->
            TreasurySetupForm form


handleTreasuryLoadingParamsMsg : TreasuryLoadingParamsMsg -> Model -> Model
handleTreasuryLoadingParamsMsg msg ({ treasuryLoadingParamsForm } as model) =
    let
        newParams params =
            { model | treasuryLoadingParamsForm = { params | error = Nothing } }

        currentUtxo =
            treasuryLoadingParamsForm.registriesSeedUtxo
    in
    case msg of
        UpdatePragmaScriptHash value ->
            newParams { treasuryLoadingParamsForm | pragmaScriptHash = value }

        UpdateRegistriesSeedTransactionId value ->
            newParams { treasuryLoadingParamsForm | registriesSeedUtxo = { currentUtxo | transactionId = value } }

        UpdateRegistriesSeedOutputIndex value ->
            case String.toInt value of
                Just outputIndex ->
                    newParams { treasuryLoadingParamsForm | registriesSeedUtxo = { currentUtxo | outputIndex = outputIndex } }

                Nothing ->
                    model

        UpdateExpiration value ->
            case String.toInt value of
                Just expiration ->
                    newParams { treasuryLoadingParamsForm | treasuryConfigExpiration = expiration }

                Nothing ->
                    model


startTreasuryLoading : UpdateContext a msg -> Model -> ( Model, Cmd msg, OutMsg )
startTreasuryLoading ctx model =
    let
        loadingTreasuryResult =
            initLoadingTreasury model.scripts model.treasuryLoadingParamsForm

        loadRegistryUtxosTask : Scopes LoadingScope -> LoadingScope -> ConcurrentTask String TaskCompleted
        loadRegistryUtxosTask loadingScopes contingencyScope =
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
                    Api.retrieveAssetsUtxos { db = ctx.db }
                        ctx.networkId
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
                |> ConcurrentTask.map LoadedRegistryUtxos
    in
    case loadingTreasuryResult of
        Ok loadingTreasury ->
            let
                allPermissionsScriptHashes =
                    Scopes.map (\s -> Tuple.first s.permissionsScriptApplied) loadingTreasury.scopes
                        |> Scopes.toList
                        |> (\list -> list ++ [ Tuple.first loadingTreasury.contingency.permissionsScriptApplied ])

                loadPermissionsRefScriptsUtxosTask : List (ConcurrentTask String TaskCompleted)
                loadPermissionsRefScriptsUtxosTask =
                    allPermissionsScriptHashes
                        |> List.map
                            (\hash ->
                                Api.retrieveScriptRefUtxos { db = ctx.db } ctx.networkId hash
                                    |> ConcurrentTask.map
                                        (\utxos ->
                                            Dict.Any.toList utxos
                                                |> List.head
                                                |> LoadedPermissionsRefScript hash
                                        )
                            )

                -- Load Pragma UTxO
                loadPragmaUtxoTask =
                    Api.retrieveAssetUtxo ctx.networkId loadingTreasury.loadingParams.pragmaScriptHash (Bytes.fromText "amaru scopes")
                        |> ConcurrentTask.mapError Debug.toString
                        |> ConcurrentTask.andThen (Api.retrieveOutput ctx.networkId)
                        |> ConcurrentTask.map LoadedPragmaUtxo

                -- Write to local storage the treasury loading params
                saveTreasuryLoadingParamsTask : ConcurrentTask String TaskCompleted
                saveTreasuryLoadingParamsTask =
                    Storage.write { db = ctx.db, storeName = "stuff" }
                        encodeLoadingParams
                        { key = "treasuryLoadingParams" }
                        loadingTreasury.loadingParams
                        |> ConcurrentTask.map (\_ -> LoadingParamsSaved)

                tasks =
                    saveTreasuryLoadingParamsTask
                        :: loadPragmaUtxoTask
                        :: loadRegistryUtxosTask loadingTreasury.scopes loadingTreasury.contingency
                        :: loadPermissionsRefScriptsUtxosTask
            in
            ( { model | treasuryManagement = TreasuryLoading loadingTreasury }
            , Cmd.none
            , OutMsg ctx.localStateUtxos tasks
            )

        Err error ->
            let
                params =
                    model.treasuryLoadingParamsForm
            in
            ( { model | treasuryLoadingParamsForm = { params | error = Just error } }
            , Cmd.none
            , OutMsg ctx.localStateUtxos []
            )


refreshTreasuryUtxos : UpdateContext a msg -> Model -> ( Model, Cmd msg, OutMsg )
refreshTreasuryUtxos ctx model =
    -- Remove all known UTxOs from the local state
    -- Reset treasury management into the loading state without treasury utxos
    -- Emit the task/command to load all treasury utxos
    case model.treasuryManagement of
        TreasuryFullyLoaded loadedTreasury ->
            let
                allTreasuriesScriptHashes =
                    (Scopes.toList loadedTreasury.scopes ++ [ loadedTreasury.contingency ])
                        |> List.map (\scope -> Tuple.first scope.sundaeTreasuryScript)

                allTreasuriesUtxosTask : ConcurrentTask String TaskCompleted
                allTreasuriesUtxosTask =
                    Api.retrieveUtxosWithPaymentCreds { db = ctx.db } ctx.networkId allTreasuriesScriptHashes
                        --> BytesMap CredentialHash (Utxo.RefDict Output)
                        |> ConcurrentTask.map bytesMapToList
                        |> ConcurrentTask.andThen listToScopes
                        |> ConcurrentTask.map LoadedTreasuriesUtxos

                -- BytesMap to List with the same order as allTreasuriesScriptHashes
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
                            ConcurrentTask.fail "Something unexpected happened while trying to retrieve treasuries UTxOs"

                allTreasuryUtxos =
                    (Scopes.toList loadedTreasury.scopes ++ [ loadedTreasury.contingency ])
                        |> List.concatMap (\scope -> Dict.Any.keys scope.treasuryUtxos)

                cleanedLocalState =
                    List.foldl Dict.Any.remove ctx.localStateUtxos allTreasuryUtxos

                loadingTreasury =
                    { loadingParams = loadedTreasury.loadingParams
                    , rootUtxo = RemoteData.Success loadedTreasury.rootUtxo
                    , scopes = Scopes.map reloadScopeTreasury loadedTreasury.scopes
                    , contingency = reloadScopeTreasury loadedTreasury.contingency
                    }

                reloadScopeTreasury scope =
                    reload loadedTreasury.loadingParams.expiration scope
                        |> (\loadingScope -> { loadingScope | treasuryUtxos = RemoteData.Loading })
            in
            ( { model | treasuryManagement = TreasuryLoading loadingTreasury }
            , Cmd.none
            , OutMsg cleanedLocalState [ allTreasuriesUtxosTask ]
            )

        _ ->
            ( model, Cmd.none, OutMsg ctx.localStateUtxos [] )


handleTreasuryMergingMsg : UpdateContext a msg -> TreasuryMergingMsg -> Model -> ( Model, Cmd msg, OutMsg )
handleTreasuryMergingMsg ctx msg model =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case msg of
        StartMergeUtxos scopeName scope rootUtxo ->
            ( handleStartMergeUtxos scopeName scope rootUtxo model, Cmd.none, noOutMsg )

        BuildMergeTransaction requiredSigners ->
            ( model
            , Task.perform (ctx.toMsg << TreasuryMergingMsg << BuildMergeTransactionWithTime requiredSigners) Time.now
            , noOutMsg
            )

        BuildMergeTransactionWithTime requiredSigners currentTime ->
            ( handleBuildMergeTransaction ctx model requiredSigners currentTime, Cmd.none, noOutMsg )

        CancelMergeAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none, noOutMsg )


handleStartMergeUtxos : String -> Scope -> OutputReference -> Model -> Model
handleStartMergeUtxos scopeName scope rootUtxo model =
    if Dict.Any.size scope.treasuryUtxos > 1 then
        let
            mergeState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = ActionIdle
                }
        in
        { model | treasuryAction = MergeTreasuryUtxos mergeState }

    else
        { model | error = Just "Scope has insufficient UTXOs to merge" }


handleBuildMergeTransaction : UpdateContext a msg -> Model -> List (Bytes CredentialHash) -> Posix -> Model
handleBuildMergeTransaction ctx model requiredSigners currentTime =
    case ( model.treasuryAction, ctx.connectedWallet ) of
        ( MergeTreasuryUtxos mergeState, Just wallet ) ->
            let
                slotConfig =
                    case ctx.networkId of
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

                ( mergeTxIntents, mergeOtherInfo ) =
                    mergeUtxos ctx.networkId mergeState.rootUtxo mergeState.scope requiredSigners validityRange

                feeSource =
                    Cip30.walletChangeAddress wallet

                txResult =
                    TxIntent.finalizeAdvanced
                        { govState = TxIntent.emptyGovernanceState
                        , localStateUtxos = ctx.localStateUtxos
                        , coinSelectionAlgo = CoinSelection.largestFirst
                        , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource mergeTxIntents
                        , costModels = Uplc.conwayDefaultCostModels
                        }
                        (TxIntent.AutoFee { paymentSource = feeSource })
                        mergeOtherInfo
                        mergeTxIntents

                updatedMergeState =
                    case txResult of
                        Ok ({ tx } as txFinalized) ->
                            { mergeState | status = AwaitingSignature (Transaction.computeTxId tx) txFinalized }

                        Err err ->
                            { mergeState | status = BuildingFailure <| TxIntent.errorToString err }

                updatedModel =
                    { model | treasuryAction = MergeTreasuryUtxos updatedMergeState }
            in
            updatedModel

        _ ->
            model



--


handleTreasuryDisburseMsg : UpdateContext a msg -> TreasuryDisburseMsg -> Model -> ( Model, Cmd msg, OutMsg )
handleTreasuryDisburseMsg ctx msg model =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case msg of
        StartDisburse startDisburseInfo ->
            ( handleStartDisburse startDisburseInfo model, Cmd.none, noOutMsg )

        CheckOwner scopeOwnerName isChecked ->
            ( handleDisburseFormUpdate (handleCheckOwner scopeOwnerName isChecked) model, Cmd.none, noOutMsg )

        AddRecipient ->
            ( handleDisburseFormUpdate addRecipient model, Cmd.none, noOutMsg )

        RemoveRecipient index ->
            ( handleDisburseFormUpdate (removeRecipient index) model, Cmd.none, noOutMsg )

        SetRecipientAddress index address ->
            ( handleDisburseFormUpdate (setRecipientAddress index address) model, Cmd.none, noOutMsg )

        SetRecipientValue index value ->
            ( handleDisburseFormUpdate (setRecipientValue index value) model, Cmd.none, noOutMsg )

        BuildDisburseTransaction ->
            ( model
            , Task.perform (ctx.toMsg << TreasuryDisburseMsg << BuildDisburseTransactionWithTime) Time.now
            , noOutMsg
            )

        BuildDisburseTransactionWithTime currentTime ->
            case ctx.connectedWallet of
                Nothing ->
                    ( { model | error = Just "Please connect your wallet" }, Cmd.none, noOutMsg )

                Just wallet ->
                    ( handleDisburseUpdate
                        (handleBuildDisburseTransaction ctx.localStateUtxos ctx.networkId wallet currentTime)
                        model
                    , Cmd.none
                    , noOutMsg
                    )

        CancelDisburseAction ->
            ( { model | treasuryAction = NoTreasuryAction }, Cmd.none, noOutMsg )


handleStartDisburse : StartDisburseInfo -> Model -> Model
handleStartDisburse { scopeName, scope, allOwners, rootUtxo, spendingUtxo } model =
    if not (Dict.Any.isEmpty scope.treasuryUtxos) then
        let
            actionState =
                { scopeName = scopeName
                , scope = scope
                , rootUtxo = rootUtxo
                , status = ActionIdle
                }

            disburseForm =
                { selectedUtxo = spendingUtxo
                , selectedScopeOwners = defaultOwnerSection
                , recipients = [ { address = "", value = (Tuple.second spendingUtxo).amount.lovelace } ]
                , error = Nothing
                }

            defaultOwnerSection =
                case scopeName of
                    "contingency" ->
                        List.map (\( name, multisig ) -> ( name, multisig, True )) allOwners

                    _ ->
                        List.map (\( name, multisig ) -> ( name, multisig, name == scopeName )) allOwners
        in
        { model | treasuryAction = Disburse actionState disburseForm, error = Nothing }

    else
        { model | error = Just <| scopeName ++ " treasury is empty. No disbursement possible." }


addRecipient : DisburseForm -> DisburseForm
addRecipient ({ recipients } as form) =
    { form | recipients = { address = "", value = N.zero } :: recipients }


removeRecipient : Int -> DisburseForm -> DisburseForm
removeRecipient index ({ recipients } as form) =
    { form | recipients = List.Extra.removeAt index recipients }


handleCheckOwner : String -> Bool -> DisburseForm -> DisburseForm
handleCheckOwner scopeOwnerName isChecked ({ selectedScopeOwners } as form) =
    let
        updatedSelection =
            List.Extra.updateIf (\( name, _, _ ) -> name == scopeOwnerName)
                (\( name, multisig, _ ) -> ( name, multisig, isChecked ))
                selectedScopeOwners
    in
    { form | selectedScopeOwners = updatedSelection }


setRecipientAddress : Int -> String -> DisburseForm -> DisburseForm
setRecipientAddress index address ({ recipients } as form) =
    let
        updatedRecipients =
            List.Extra.updateAt index
                (\recipient -> { recipient | address = address })
                recipients
    in
    { form | recipients = updatedRecipients, error = Nothing }


setRecipientValue : Int -> String -> DisburseForm -> DisburseForm
setRecipientValue index value form =
    case N.fromString value of
        Just okValue ->
            let
                updatedRecipients =
                    List.Extra.updateAt index
                        (\recipient -> { recipient | value = okValue })
                        form.recipients
            in
            { form | recipients = updatedRecipients, error = Nothing }

        Nothing ->
            { form | error = Just <| "Invalid value: " ++ value }


handleDisburseFormUpdate : (DisburseForm -> DisburseForm) -> Model -> Model
handleDisburseFormUpdate formUpdate model =
    handleDisburseUpdate (\state form -> Disburse state <| formUpdate form) model


handleDisburseUpdate : (ScopeActionState -> DisburseForm -> TreasuryAction) -> Model -> Model
handleDisburseUpdate disburseUpdate model =
    case model.treasuryAction of
        Disburse disburseState form ->
            { model | treasuryAction = disburseUpdate disburseState form, error = Nothing }

        _ ->
            model


handleBuildDisburseTransaction : Utxo.RefDict Output -> NetworkId -> Cip30.Wallet -> Posix -> ScopeActionState -> DisburseForm -> TreasuryAction
handleBuildDisburseTransaction localStateUtxos networkId wallet currentTime disburseState form =
    let
        spentUtxo =
            form.selectedUtxo

        -- Extract signers from the scope owners.
        -- TODO: actually we should let finer grained control from users in the form.
        requiredSigners =
            form.selectedScopeOwners
                |> List.filterMap
                    (\( _, multisig, isChecked ) ->
                        if isChecked then
                            Just <| MultisigScript.extractRequiredSigners multisig

                        else
                            Nothing
                    )
                |> List.concat

        recipients : List TxIntent
        recipients =
            form.recipients
                |> List.filterMap
                    (\{ address, value } ->
                        Address.fromString address
                            |> Maybe.map (\addr -> TxIntent.SendTo addr <| Value.onlyLovelace value)
                    )

        totalValueSpent =
            form.recipients
                |> List.foldl (\{ value } -> N.add value) N.zero
                |> Value.onlyLovelace

        -- Continue building the Tx
        slotConfig =
            case networkId of
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

        -- ( disburseTxIntents, disburseOtherInfo )
        disburseIntentsResult =
            disburse networkId disburseState.rootUtxo disburseState.scope requiredSigners validityRange spentUtxo (\_ -> recipients) totalValueSpent

        feeSource =
            Cip30.walletChangeAddress wallet

        txResult =
            disburseIntentsResult
                |> Result.andThen
                    (\( disburseTxIntents, disburseOtherInfo ) ->
                        TxIntent.finalizeAdvanced
                            { govState = TxIntent.emptyGovernanceState
                            , localStateUtxos = localStateUtxos
                            , coinSelectionAlgo = CoinSelection.largestFirst
                            , evalScriptsCosts = TxIntent.defaultEvalScriptsCosts feeSource disburseTxIntents
                            , costModels = Uplc.conwayDefaultCostModels
                            }
                            (TxIntent.AutoFee { paymentSource = feeSource })
                            disburseOtherInfo
                            disburseTxIntents
                            |> Result.mapError TxIntent.errorToString
                    )

        updatedDisburseState =
            case txResult of
                Ok ({ tx } as txFinalized) ->
                    { disburseState | status = AwaitingSignature (Transaction.computeTxId tx) txFinalized }

                Err err ->
                    { disburseState | status = BuildingFailure err }
    in
    Disburse updatedDisburseState form


{-| Disburse funds from one UTxO in the given scope.
-}
disburse : NetworkId -> OutputReference -> Scope -> List (Bytes CredentialHash) -> Maybe { start : Int, end : Natural } -> ( OutputReference, Output ) -> (Value -> List TxIntent) -> Value -> Result String ( List TxIntent, List TxOtherInfo )
disburse networkId rootUtxoRef scope requiredSigners validityRange ( spentUtxoRef, spentOutput ) receivers value =
    let
        ( _, treasuryScript ) =
            scope.sundaeTreasuryScript

        treasuryWitnessSource =
            case scope.sundaeTreasuryScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes treasuryScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        spendConfig : SpendConfig
        spendConfig =
            { scriptWitnessSource = treasuryWitnessSource
            , registryOutputRef = Tuple.first scope.registryUtxo
            , requiredSigners = requiredSigners
            , requiredWithdrawals = requiredWithdrawals
            , spentInputRef = spentUtxoRef
            , spentOutput = spentOutput
            , validityRange = validityRange
            }

        requiredWithdrawals =
            -- Withdrawal with the permissions script
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
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ( permissionsScriptHash, permissionsScript ) =
            scope.permissionsScript

        permissionsWitnessSource =
            case scope.permissionsScriptRef of
                Nothing ->
                    Witness.ByValue <| Script.cborWrappedBytes permissionsScript

                Just ( ref, _ ) ->
                    Witness.ByReference ref

        overflowValue =
            Value.subtract value spentOutput.amount

        ( txIntents, otherIntents ) =
            Treasury.disburse spendConfig receivers value
    in
    if overflowValue == Value.zero then
        Ok <| ( txIntents, TxIntent.TxReferenceInput rootUtxoRef :: otherIntents )

    else
        Err <| "Trying to disburse more than is available in this UTxO. Overflow value is: " ++ Debug.toString overflowValue



--


createPublishScriptTx : UpdateContext a msg -> Bytes CredentialHash -> PlutusScript -> Model -> ( Model, Cmd msg, OutMsg )
createPublishScriptTx ctx _ script model =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case ctx.connectedWallet of
        Nothing ->
            ( { model | error = Just "Please connect your wallet to be able to create a Tx" }, Cmd.none, noOutMsg )

        Just wallet ->
            let
                walletAddress =
                    Cip30.walletChangeAddress wallet

                walletKey =
                    Address.extractPubKeyHash walletAddress
                        |> Maybe.withDefault (Bytes.dummy 28 "")

                -- For now this is published in a multisig with just the wallet key as payment credential,
                -- and re-using the same stake credential to keep it staked.
                utxoAddress =
                    Address.script ctx.networkId (Script.hash <| Script.Native <| ScriptPubkey walletKey)
                        |> Address.setShelleyStakeCred (Address.extractStakeCredential walletAddress)

                scriptUtxo =
                    Utxo.withMinAda <|
                        { address = utxoAddress
                        , amount = Value.zero
                        , datumOption = Nothing
                        , referenceScript = Just <| Script.refFromScript <| Script.Plutus script
                        }

                publishIntents =
                    [ TxIntent.Spend <|
                        TxIntent.FromWallet
                            { address = walletAddress
                            , value = scriptUtxo.amount
                            , guaranteedUtxos = []
                            }
                    , TxIntent.SendToOutput scriptUtxo
                    ]

                txResult =
                    TxIntent.finalize ctx.localStateUtxos [] publishIntents
            in
            case txResult of
                Err err ->
                    ( { model | error = Just <| TxIntent.errorToString err }, Cmd.none, noOutMsg )

                Ok { tx, expectedSignatures } ->
                    let
                        expectedSigners =
                            List.map (\keyHash -> { keyHash = keyHash, keyName = "Unidentified signer" }) expectedSignatures

                        signingRoute =
                            { tx = Just tx, expectedSigners = expectedSigners }
                    in
                    ( model, Cmd.Extra.perform <| ctx.routingConfig.urlChangedMsg <| Route.Signing signingRoute, noOutMsg )



-- CompletedTask


handleCompletedTask : UpdateContext a msg -> TaskCompleted -> Model -> ( Model, Cmd msg, OutMsg )
handleCompletedTask ctx taskCompleted model =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case taskCompleted of
        LoadingParamsSaved ->
            ( model, Cmd.none, noOutMsg )

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
                                    Api.retrieveUtxosWithPaymentCreds { db = ctx.db } ctx.networkId allTreasuriesScriptHashes

                                allTreasuriesScriptHashes =
                                    List.filterMap extractAppliedTreasuryScriptHash loadingScopes

                                loadingScopes =
                                    case treasuryManagement of
                                        TreasuryLoading { scopes, contingency } ->
                                            -- The list order is the same as the order of the scopes
                                            Scopes.toList scopes ++ [ contingency ]

                                        _ ->
                                            []

                                extractAppliedTreasuryScriptHash { sundaeTreasuryScriptApplied } =
                                    RemoteData.map (\( hash, _ ) -> hash) sundaeTreasuryScriptApplied
                                        |> RemoteData.toMaybe

                                loadTreasuriesUtxosTask =
                                    allTreasuriesUtxosTask
                                        |> ConcurrentTask.map bytesMapToList
                                        |> ConcurrentTask.andThen listToScopes
                                        |> ConcurrentTask.map LoadedTreasuriesUtxos

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

                                allTreasuryRefScriptsUtxosTask : List (ConcurrentTask String TaskCompleted)
                                allTreasuryRefScriptsUtxosTask =
                                    allTreasuriesScriptHashes
                                        |> List.map
                                            (\hash ->
                                                Api.retrieveScriptRefUtxos { db = ctx.db } ctx.networkId hash
                                                    |> ConcurrentTask.map
                                                        (\utxos ->
                                                            Dict.Any.toList utxos
                                                                |> List.head
                                                                |> LoadedTreasuryRefScript hash
                                                        )
                                            )
                            in
                            ( { model | treasuryManagement = treasuryManagement }
                            , Cmd.none
                            , OutMsg ctx.localStateUtxos (loadTreasuriesUtxosTask :: allTreasuryRefScriptsUtxosTask)
                            )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none, noOutMsg )

                _ ->
                    ( model, Cmd.none, noOutMsg )

        LoadedRegistryUtxos ( registryUtxos, contingencyUtxo ) ->
            updateTreasuryLoading ctx.localStateUtxos model <|
                \({ scopes, contingency } as loadingTreasury) ->
                    { loadingTreasury
                        | scopes = setRegistryUtxos registryUtxos scopes
                        , contingency = { contingency | registryUtxo = RemoteData.Success contingencyUtxo }
                    }

        LoadedTreasuriesUtxos ( treasuriesUtxos, contingencyUtxos ) ->
            updateTreasuryLoading ctx.localStateUtxos model <|
                \({ scopes, contingency } as loadingTreasury) ->
                    { loadingTreasury
                        | scopes = setTreasuryUtxos treasuriesUtxos scopes
                        , contingency = { contingency | treasuryUtxos = RemoteData.Success contingencyUtxos }
                    }

        LoadedTreasuryRefScript scriptHash maybeUtxo ->
            let
                updateTreasuryRefScriptIfGoodScope scope =
                    -- This should always happen at a time where the treasury scripts are known,
                    -- so we should always be in a RemoteData.Success state.
                    case scope.sundaeTreasuryScriptApplied of
                        RemoteData.Success ( hash, _ ) ->
                            if hash == scriptHash then
                                { scope | sundaeTreasuryScriptPublished = RemoteData.Success maybeUtxo }

                            else
                                scope

                        _ ->
                            scope
            in
            updateTreasuryLoading ctx.localStateUtxos model <|
                \({ scopes, contingency } as loadingTreasury) ->
                    { loadingTreasury
                        | scopes = Scopes.map updateTreasuryRefScriptIfGoodScope scopes
                        , contingency = updateTreasuryRefScriptIfGoodScope contingency
                    }

        LoadedPermissionsRefScript scriptHash maybeUtxo ->
            let
                updatePermissionsRefScriptIfGoodScope scope =
                    if scriptHash == Tuple.first scope.permissionsScriptApplied then
                        { scope | permissionsScriptPublished = RemoteData.Success maybeUtxo }

                    else
                        scope
            in
            updateTreasuryLoading ctx.localStateUtxos model <|
                \({ scopes, contingency } as loadingTreasury) ->
                    { loadingTreasury
                        | scopes = Scopes.map updatePermissionsRefScriptIfGoodScope scopes
                        , contingency = updatePermissionsRefScriptIfGoodScope contingency
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
            , expiration = N.fromSafeInt <| Time.posixToMillis scope.expiration
            , payoutUpperbound = N.zero
            }
    in
    Treasury.initializeScript treasuryConfig unappliedScript
        |> Result.Extra.unpack RemoteData.Failure
            (\applied -> RemoteData.Success ( Script.hash <| Script.Plutus applied, applied ))


setRegistryUtxos : Scopes ( OutputReference, Output ) -> Scopes LoadingScope -> Scopes LoadingScope
setRegistryUtxos registryUtxos scopes =
    Scopes.map2 (\utxo loadingScope -> { loadingScope | registryUtxo = RemoteData.Success utxo }) registryUtxos scopes


setTreasuryUtxos : Scopes (Utxo.RefDict Output) -> Scopes LoadingScope -> Scopes LoadingScope
setTreasuryUtxos treasuryUtxos scopes =
    Scopes.map2 (\utxos loadingScope -> { loadingScope | treasuryUtxos = RemoteData.Success utxos }) treasuryUtxos scopes


updateTreasuryLoading : Utxo.RefDict Output -> Model -> (LoadingTreasury -> LoadingTreasury) -> ( Model, Cmd msg, OutMsg )
updateTreasuryLoading localStateUtxos model f =
    case model.treasuryManagement of
        TreasuryLoading loadingTreasury ->
            let
                ( updatedTreasuryManagement, newUtxos ) =
                    TreasuryLoading (f loadingTreasury)
                        -- If everything has loaded,
                        -- then we can convert into a LoadedTreasury
                        |> upgradeIfTreasuryLoadingFinished localStateUtxos

                ( finalTreasuryManagement, finalForm ) =
                    doubleCheckTreasuryScriptsHashes updatedTreasuryManagement model.treasuryLoadingParamsForm
            in
            ( { model
                | treasuryManagement = finalTreasuryManagement
                , treasuryLoadingParamsForm = finalForm
              }
            , Cmd.none
            , OutMsg newUtxos []
            )

        _ ->
            ( model, Cmd.none, OutMsg localStateUtxos [] )



--


updateWithTx : Utxo.RefDict Output -> Maybe { a | txId : Bytes TransactionId, tx : Transaction } -> Model -> ( Model, OutMsg )
updateWithTx localStateUtxos maybeTx ({ treasuryManagement, treasuryAction } as model) =
    case maybeTx of
        Nothing ->
            ( model, OutMsg localStateUtxos [] )

        Just { txId, tx } ->
            let
                updatedLocalState =
                    (TxIntent.updateLocalState txId tx localStateUtxos).updatedState
            in
            case treasuryManagement of
                TreasurySetupTxs setupState ->
                    ( { model
                        | treasuryManagement =
                            markTxAsSubmitted txId setupState
                                |> upgradeToLoadedIfSetupIsDone
                        , treasuryAction = updateTreasuryActionWithTx txId treasuryAction
                      }
                    , OutMsg updatedLocalState []
                    )

                TreasuryFullyLoaded loadedTreasury ->
                    ( { model
                        | treasuryManagement =
                            updateTreasuryUtxos txId tx loadedTreasury
                                |> TreasuryFullyLoaded
                        , treasuryAction = updateTreasuryActionWithTx txId treasuryAction
                      }
                    , OutMsg updatedLocalState []
                    )

                -- No other management state should trigger a Tx.
                -- We just update the local state in case it’s a Tx unrelated to treasury management.
                _ ->
                    ( model, OutMsg updatedLocalState [] )


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


updateTreasuryUtxos : Bytes TransactionId -> Transaction -> LoadedTreasury -> LoadedTreasury
updateTreasuryUtxos txId tx loadedTreasury =
    -- TODO: Detect all changes to the treasury happening in the Tx
    loadedTreasury


updateTreasuryActionWithTx : Bytes TransactionId -> TreasuryAction -> TreasuryAction
updateTreasuryActionWithTx txId treasuryAction =
    let
        maybeTxIdWait =
            case treasuryAction of
                MergeTreasuryUtxos { status } ->
                    case status of
                        AwaitingSignature txIdWait _ ->
                            Just txIdWait

                        _ ->
                            Nothing

                Disburse { status } _ ->
                    case status of
                        AwaitingSignature txIdWait _ ->
                            Just txIdWait

                        _ ->
                            Nothing

                NoTreasuryAction ->
                    Nothing
    in
    case Maybe.map ((==) txId) maybeTxIdWait of
        Just True ->
            NoTreasuryAction

        _ ->
            treasuryAction



-- VIEW ##############################################################


type alias ViewContext a msg =
    { a
        | toMsg : Msg -> msg
        , routingConfig : Route.Config msg
        , networkId : NetworkId
        , connectedWallet : Maybe Cip30.Wallet
    }


view : ViewContext a msg -> Model -> Html msg
view ctx model =
    case model.treasuryAction of
        NoTreasuryAction ->
            viewTreasurySection ctx model.treasuryLoadingParamsForm model.treasuryManagement

        MergeTreasuryUtxos mergeState ->
            viewMergeUtxosAction ctx mergeState

        Disburse disburseState form ->
            viewDisburseAction ctx disburseState form


viewTreasurySection : ViewContext a msg -> TreasuryLoadingParamsForm -> TreasuryManagement -> Html msg
viewTreasurySection ({ toMsg, networkId } as ctx) params treasuryManagement =
    case treasuryManagement of
        TreasuryUnspecified ->
            Html.map toMsg <|
                div []
                    [ Html.h2 [] [ Html.text "Setup a new treasury" ]
                    , Html.button [ onClick StartTreasurySetup ]
                        [ text "Setup a new treasury" ]
                    , Html.h2 [] [ Html.text "Load an existing treasury" ]
                    , Html.p [] [ text "With the following parameters:" ]
                    , Html.p []
                        [ Html.label [] [ text "Pragma Scopes script hash: " ]
                        , Html.input
                            [ HA.type_ "text"
                            , HA.placeholder "44dd0678ba5f89b41869362ea3d3e509f94e48bd57be57faedaad0c6"
                            , HA.value params.pragmaScriptHash
                            , HE.onInput (TreasuryLoadingParamsMsg << UpdatePragmaScriptHash)
                            ]
                            []
                        ]
                    , Html.p []
                        [ Html.label [] [ text "Registries Seed UTxO - Tx ID: " ]
                        , Html.input
                            [ HA.type_ "text"
                            , HA.placeholder "9737488cbd45bc71d5c12490865b20ec8aa9f74b3110f4c2aa588b5e1e09dad6"
                            , HA.value params.registriesSeedUtxo.transactionId
                            , HE.onInput (TreasuryLoadingParamsMsg << UpdateRegistriesSeedTransactionId)
                            ]
                            []
                        , Html.label [] [ text " - Output Index: # " ]
                        , Html.input
                            [ HA.type_ "number"
                            , HA.size 2
                            , HA.min "0"
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
                    , Html.button [ onClick StartTreasuryLoading ]
                        [ text "Load treasury" ]
                    , viewError params.error
                    ]

        TreasurySetupForm form ->
            Html.map toMsg <|
                SetupForm.view { toMsg = UpdateSetupForm, tryBuildSetupTxs = TryBuildSetupTxs } form

        TreasurySetupTxs state ->
            viewSetupTxsState ctx state

        TreasuryLoading { rootUtxo, loadingParams, scopes, contingency } ->
            div []
                [ Html.p [] [ text "Loading treasury ... ", spinner ]
                , viewLoadingRootUtxo rootUtxo
                , viewPragmaScopesScriptHash loadingParams.pragmaScriptHash
                , viewRegistriesSeedUtxo loadingParams.registriesSeedUtxo
                , viewExpirationDate <| Time.posixToMillis loadingParams.expiration
                , viewLoadingScope "ledger" scopes.ledger
                , viewLoadingScope "consensus" scopes.consensus
                , viewLoadingScope "mercenaries" scopes.mercenaries
                , viewLoadingScope "marketing" scopes.marketing
                , viewLoadingScope "contingency" contingency
                ]

        TreasuryFullyLoaded { rootUtxo, loadingParams, scopes, contingency } ->
            let
                rootUtxoRef =
                    Tuple.first rootUtxo

                allOwners =
                    Scopes.map .owner scopes
                        |> Scopes.toList
                        |> List.Extra.zip [ "ledger", "consensus", "mercenaries", "marketing" ]
            in
            div []
                [ Html.p [] [ text "Treasury fully loaded" ]
                , viewReload loadingParams
                , viewRootUtxo rootUtxo
                , Html.map toMsg <| viewScope networkId rootUtxoRef allOwners "ledger" scopes.ledger
                , Html.map toMsg <| viewScope networkId rootUtxoRef allOwners "consensus" scopes.consensus
                , Html.map toMsg <| viewScope networkId rootUtxoRef allOwners "mercenaries" scopes.mercenaries
                , Html.map toMsg <| viewScope networkId rootUtxoRef allOwners "marketing" scopes.marketing
                , Html.map toMsg <| viewScope networkId rootUtxoRef allOwners "contingency" contingency
                ]


viewReload : LoadingParams -> Html msg
viewReload { pragmaScriptHash, registriesSeedUtxo, expiration } =
    Html.div [ HA.style "padding" "16px", HA.style "box-shadow" "0 0 16px rgba(0, 0, 0, 0.2)" ]
        [ Html.p [] [ Html.strong [] [ text "KEEP this to be able to reload the Treasury:" ] ]
        , viewPragmaScopesScriptHash pragmaScriptHash
        , viewRegistriesSeedUtxo registriesSeedUtxo
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


viewSetupTxsState : ViewContext a msg -> SetupTxsState -> Html msg
viewSetupTxsState ctx { txs, treasury, tracking } =
    div []
        [ Html.h2 [] [ text "Treasury State after Initialization" ]
        , viewRootUtxo treasury.rootUtxo
        , Html.map ctx.toMsg <| viewScopeSetup "ledger" treasury.scopes.ledger
        , Html.map ctx.toMsg <| viewScopeSetup "consensus" treasury.scopes.consensus
        , Html.map ctx.toMsg <| viewScopeSetup "mercenaries" treasury.scopes.mercenaries
        , Html.map ctx.toMsg <| viewScopeSetup "marketing" treasury.scopes.marketing
        , Html.map ctx.toMsg <| viewScopeSetup "contingency" treasury.contingency
        , Html.h2 [] [ text "Txs to submit for Treasury Initialization" ]
        , Html.h3 [] [ text "Scope Owners Definition" ]
        , viewTxStatus ctx tracking.scopes txs.scopes
        , Html.h3 [] [ text "Permissions Stake Registration" ]
        , viewTxStatus ctx tracking.permissions txs.permissions
        , Html.h3 [] [ text "Registries Initializations" ]
        , viewTxStatus ctx tracking.registries txs.registries
        ]


viewTxStatus : ViewContext a msg -> TxState -> SignTx.Prep -> Html msg
viewTxStatus { routingConfig } txState ({ txId, tx, expectedSignatures, signerDescriptions } as prep) =
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
                    Html.button []
                        [ SignTx.signingLink routingConfig prep [] [ text "Sign on signing page" ] ]

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
viewLoadingScope scopeName { owner, permissionsScriptApplied, permissionsScriptPublished, sundaeTreasuryScriptApplied, sundaeTreasuryScriptPublished, registryNftPolicyId, registryUtxo, treasuryUtxos } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewMaybeOwner owner
        , viewPermissionsScript permissionsScriptApplied
        , viewLoadingPermissionsScriptRef permissionsScriptPublished
        , viewLoadingTreasuryScript sundaeTreasuryScriptApplied
        , viewLoadingTreasuryScriptRef sundaeTreasuryScriptPublished
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


viewScope : NetworkId -> OutputReference -> List ( String, MultisigScript ) -> String -> Scope -> Html Msg
viewScope networkId rootUtxo allOwners scopeName ({ owner, permissionsScript, permissionsScriptRef, sundaeTreasuryScript, sundaeTreasuryScriptRef, registryUtxo, treasuryUtxos } as scope) =
    let
        treasuryScriptHash =
            Tuple.first sundaeTreasuryScript

        treasuryAddress =
            Address.base networkId (Address.ScriptHash treasuryScriptHash) (Address.ScriptHash treasuryScriptHash)
    in
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , text <| "Scope address: " ++ Address.toBech32 treasuryAddress
        , viewOwner owner
        , viewPermissionsScript permissionsScript
        , viewPermissionsScriptRef (Tuple.first permissionsScript) (Tuple.second permissionsScript) permissionsScriptRef
        , viewTreasuryScript sundaeTreasuryScript
        , viewTreasuryScriptRef (Tuple.first sundaeTreasuryScript) (Tuple.second sundaeTreasuryScript) sundaeTreasuryScriptRef
        , viewRegistryUtxo registryUtxo
        , viewTreasuryUtxos scopeName scope allOwners rootUtxo treasuryUtxos
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


viewLoadingPermissionsScriptRef : RemoteData String (Maybe ( OutputReference, Output )) -> Html msg
viewLoadingPermissionsScriptRef remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Permissions script ref not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Permissions script ref loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Permissions script ref failed to load: " ++ error ]

        RemoteData.Success maybeUtxo ->
            case maybeUtxo of
                Just ( ref, _ ) ->
                    Html.p [] [ text <| "Permissions script ref UTxO: " ++ Utxo.refAsString ref ]

                Nothing ->
                    Html.p [] [ text <| "Permissions script ref not published yet." ]


viewPermissionsScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewPermissionsScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied permissions script hash: " ++ Bytes.toHex hash ]


viewPermissionsScriptRef : Bytes CredentialHash -> PlutusScript -> Maybe ( OutputReference, Output ) -> Html Msg
viewPermissionsScriptRef hash script maybeUtxo =
    case maybeUtxo of
        Just ( ref, _ ) ->
            Html.p [] [ text <| "Permissions script ref UTxO: " ++ Utxo.refAsString ref ]

        Nothing ->
            Html.p []
                [ text <| "Permissions script ref not published yet. "
                , Html.button [ onClick <| PublishScript hash script ] [ text "Publish script in a ref UTxO" ]
                ]


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


viewLoadingTreasuryScriptRef : RemoteData String (Maybe ( OutputReference, Output )) -> Html msg
viewLoadingTreasuryScriptRef remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Sundae treasury script ref not asked yet" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Sundae treasury script ref loading ... ", spinner ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Sundae treasury script ref failed to load: " ++ error ]

        RemoteData.Success maybeUtxo ->
            case maybeUtxo of
                Just ( ref, _ ) ->
                    Html.p [] [ text <| "Treasury script ref UTxO: " ++ Utxo.refAsString ref ]

                Nothing ->
                    Html.p [] [ text <| "Treasury script ref not published yet." ]


viewTreasuryScriptRef : Bytes CredentialHash -> PlutusScript -> Maybe ( OutputReference, Output ) -> Html Msg
viewTreasuryScriptRef hash script maybeUtxo =
    case maybeUtxo of
        Just ( ref, _ ) ->
            Html.p [] [ text <| "Treasury script ref UTxO: " ++ Utxo.refAsString ref ]

        Nothing ->
            Html.p []
                [ text <| "Treasury script ref not published yet. "
                , Html.button [ onClick <| PublishScript hash script ] [ text "Publish script in a ref UTxO" ]
                ]


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


viewTreasuryUtxos : String -> Scope -> List ( String, MultisigScript ) -> OutputReference -> Utxo.RefDict Output -> Html Msg
viewTreasuryUtxos scopeName scope allOwners rootUtxo utxos =
    let
        refreshUtxosButton =
            Html.button [ onClick RefreshTreasuryUtxos ] [ text "Refresh UTxOs" ]

        utxosCount =
            Dict.Any.size utxos

        mergeButton =
            if utxosCount > 1 then
                Html.button [ onClick (TreasuryMergingMsg <| StartMergeUtxos scopeName scope rootUtxo) ] [ text "Merge UTxOs" ]

            else
                text ""

        startDisburseInfo : ( OutputReference, Output ) -> StartDisburseInfo
        startDisburseInfo utxo =
            { scopeName = scopeName
            , scope = scope
            , allOwners = allOwners
            , rootUtxo = rootUtxo
            , spendingUtxo = utxo
            }

        disburseButton utxo =
            Html.button [ onClick (TreasuryDisburseMsg <| StartDisburse <| startDisburseInfo utxo) ] [ text "Disburse" ]

        viewDetailedUtxoItem utxo =
            Html.li []
                (viewDetailedUtxo utxo ++ [ disburseButton utxo ])
    in
    div []
        [ Html.p [] [ text <| "Treasury UTxOs count: " ++ String.fromInt (Dict.Any.size utxos) ]
        , Html.p [] [ text <| "TODO: add buttons for possible actions with those UTxOs" ]
        , refreshUtxosButton
        , mergeButton
        , Html.ul [] <|
            List.map viewDetailedUtxoItem (Dict.Any.toList utxos)
        ]


viewDetailedUtxo : ( OutputReference, Output ) -> List (Html msg)
viewDetailedUtxo ( ref, output ) =
    [ div [] [ text <| "UTxO: " ++ Utxo.refAsString ref ]
    , div [] [ text <| "Address: " ++ Address.toBech32 output.address ]
    , div [] [ text <| "Value: (₳ amounts are in Lovelaces)" ]
    , Html.pre [] [ text <| String.join "\n" <| Value.toMultilineString output.amount ]
    ]


spinner : Html msg
spinner =
    Html.span [ HA.class "loader" ] []



-- Merge UTxOs


viewMergeUtxosAction : ViewContext a msg -> ScopeActionState -> Html msg
viewMergeUtxosAction { toMsg, routingConfig, connectedWallet } mergeState =
    div []
        [ Html.h3 [] [ text ("Merge UTXOs - " ++ mergeState.scopeName ++ " Scope") ]
        , div [] [ text ("UTXOs to merge: " ++ String.fromInt (Dict.Any.size mergeState.scope.treasuryUtxos)) ]
        , Html.ul [] <|
            List.map (Html.li [] << viewDetailedUtxo) (Dict.Any.toList mergeState.scope.treasuryUtxos)
        , case mergeState.status of
            ActionIdle ->
                let
                    -- FIXME: temporary, something more robust should be done,
                    -- and not inside the view, but this works as a MVP.
                    requiredSigners =
                        MultisigScript.extractRequiredSigners mergeState.scope.owner
                in
                Html.map (toMsg << TreasuryMergingMsg) <|
                    case connectedWallet of
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

            BuildingFailure error ->
                Html.map (toMsg << TreasuryMergingMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelMergeAction ] [ text "Cancel" ]
                        ]

            AwaitingSignature txId { tx, expectedSignatures } ->
                let
                    signingPrep =
                        { txId = txId
                        , tx = tx
                        , expectedSignatures = expectedSignatures

                        -- signerDescriptions : BytesMap CredentialHash String
                        , signerDescriptions = Bytes.Map.empty
                        }
                in
                div []
                    [ Html.p [] [ text "Transaction built successfully." ]
                    , Html.pre [] [ text <| prettyTx tx ]
                    , Html.button []
                        [ SignTx.signingLink routingConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryMergingMsg CancelMergeAction ] [ text "Cancel" ]
                    ]
        ]



-- Disburse


viewDisburseAction : ViewContext a msg -> ScopeActionState -> DisburseForm -> Html msg
viewDisburseAction { toMsg, routingConfig, connectedWallet } actionState form =
    div []
        [ Html.h3 [] [ text ("Disburse - " ++ actionState.scopeName ++ " Scope") ]
        , case actionState.status of
            ActionIdle ->
                Html.map (toMsg << TreasuryDisburseMsg) <|
                    case connectedWallet of
                        Nothing ->
                            div []
                                [ Html.p [] [ text "Please connect your wallet, it will be used to pay the Tx fees." ]
                                , Html.button [ onClick CancelDisburseAction ] [ text "Cancel" ]
                                ]

                        Just _ ->
                            div []
                                [ viewPickedUtxo form.selectedUtxo
                                , viewSecondSignerPicker actionState.scopeName form.selectedScopeOwners
                                , viewRecipientsSection form
                                , Html.h4 [] [ text "Tx building" ]
                                , Html.button [ onClick BuildDisburseTransaction ] [ text "Build Transaction" ]
                                , Html.button [ onClick CancelDisburseAction ] [ text "Cancel" ]
                                , viewError form.error
                                ]

            BuildingFailure error ->
                Html.map (toMsg << TreasuryDisburseMsg) <|
                    div []
                        [ Html.p [] [ text "Failed to build the Tx, with error:" ]
                        , Html.pre [] [ text error ]
                        , Html.button [ onClick CancelDisburseAction ] [ text "Cancel" ]
                        ]

            AwaitingSignature txId { tx, expectedSignatures } ->
                let
                    signingPrep =
                        { txId = txId
                        , tx = tx
                        , expectedSignatures = expectedSignatures

                        -- signerDescriptions : BytesMap CredentialHash String
                        , signerDescriptions = Bytes.Map.empty
                        }
                in
                div []
                    [ Html.p [] [ text "Transaction built successfully." ]
                    , Html.pre [] [ text <| prettyTx tx ]
                    , Html.button []
                        [ SignTx.signingLink routingConfig signingPrep [] [ text "Sign on signing page" ] ]
                    , Html.button [ onClick <| toMsg <| TreasuryDisburseMsg CancelDisburseAction ] [ text "Cancel" ]
                    ]
        ]


viewPickedUtxo : ( OutputReference, Output ) -> Html msg
viewPickedUtxo utxo =
    Html.p []
        (Html.p [] [ text "Picked UTxO for spending:" ] :: viewDetailedUtxo utxo)


viewSecondSignerPicker : String -> List ( String, MultisigScript, Bool ) -> Html TreasuryDisburseMsg
viewSecondSignerPicker scopeName selectedScopeOwners =
    case scopeName of
        "contingency" ->
            Html.p []
                [ text "All scope owners must approve a disburse action for the contingency scope."
                , Html.ul [] (List.map (\owner -> Html.li [] [ viewScopeOwner owner ]) selectedScopeOwners)
                ]

        _ ->
            Html.p []
                [ Html.p [] [ text "You and at least one other scope owner must approve a disburse action. Pick the signers:" ]
                , div [] (List.map (viewOwnerCheckbox scopeName) selectedScopeOwners)
                ]


viewOwnerCheckbox : String -> ( String, MultisigScript, Bool ) -> Html TreasuryDisburseMsg
viewOwnerCheckbox disbursingScopeName ( name, multisig, checked ) =
    let
        attributes =
            if name == disbursingScopeName then
                -- The disbursing scope must sign, it cannot be unchecked
                [ HA.checked True, HA.disabled True ]

            else
                [ HA.checked checked, HE.onCheck <| CheckOwner name ]
    in
    div []
        [ Html.label []
            [ Html.input (HA.type_ "checkbox" :: attributes) []
            , viewScopeOwner ( name, multisig, () )
            ]
        ]


viewScopeOwner : ( String, MultisigScript, a ) -> Html msg
viewScopeOwner ( name, multisig, _ ) =
    text <| name ++ ": " ++ Debug.toString multisig


viewRecipientsSection : DisburseForm -> Html TreasuryDisburseMsg
viewRecipientsSection { recipients } =
    Html.div []
        [ Html.h4 [] [ text "Recipients" ]
        , Html.p []
            [ text "Paste the recipient’s address and value: "
            , Html.button [ onClick AddRecipient ] [ text "add recipient" ]
            ]
        , div [] (List.indexedMap viewRecipient recipients)
        ]


viewRecipient : Int -> { address : String, value : Natural } -> Html TreasuryDisburseMsg
viewRecipient index { address, value } =
    div []
        [ text <| "Recipient " ++ String.fromInt (index + 1) ++ ": "
        , Html.label []
            [ Html.input [ HA.type_ "text", HA.placeholder "paste address", HA.value address, HE.onInput (SetRecipientAddress index) ] [] ]
        , text " "
        , Html.label []
            [ Html.input [ HA.type_ "number", HA.placeholder "value", HA.value (N.toString value), HE.onInput (SetRecipientValue index) ] [] ]
        , text " "
        , Html.button [ onClick (RemoveRecipient index) ] [ text "remove" ]
        ]
