module Page.Loading exposing (Context, Loaded, Loading, LoadingScope, OutMsg, TaskCompleted(..), doubleCheckTreasuryScriptsHashes, refreshUtxos, startLoading, updateWithCompletedTask, upgradeIfTreasuryLoadingFinished, view, viewRootUtxo)

import Api
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Script as Script exposing (PlutusScript)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import ConcurrentTask exposing (ConcurrentTask)
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Json.Decode as JD
import MultisigScript exposing (MultisigScript)
import Natural as N
import RemoteData exposing (RemoteData)
import Result.Extra
import Storage
import Time exposing (Posix)
import Treasury.LoadingParams as LoadingParams exposing (LoadingParams)
import Treasury.Scope exposing (Scope, Scripts, viewOwner, viewPermissionsScript, viewRegistryUtxo, viewTreasuryScript)
import Treasury.Scopes as Scopes exposing (Scopes)
import Treasury.Sundae
import Treasury.SundaeTypes as SundaeTypes
import Utils exposing (spinner)


type alias Loading =
    { loadingParams : LoadingParams
    , rootUtxo : RemoteData String ( OutputReference, Output )
    , scopes : Scopes LoadingScope
    , contingency : LoadingScope
    }


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


type alias Loaded =
    { rootUtxo : ( OutputReference, Output )
    , loadingParams : LoadingParams
    , scopes : Scopes Scope
    , contingency : Scope
    }


initLoading : Scripts -> LoadingParams.Form -> Result String Loading
initLoading unappliedScripts formParams =
    LoadingParams.validate formParams
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



-- UPDATE ############################################################


type alias Context a =
    { a
        | db : JD.Value
        , localStateUtxos : Utxo.RefDict Output
        , networkId : NetworkId
    }


type alias OutMsg =
    { updatedLocalState : Utxo.RefDict Output
    , runTasks : List (ConcurrentTask String TaskCompleted)
    }



-- Init


startLoading : Context a -> Scripts -> LoadingParams.Form -> Result String ( Loading, OutMsg )
startLoading ctx scripts form =
    let
        loadRegistryUtxosTask : Scopes LoadingScope -> LoadingScope -> ConcurrentTask String TaskCompleted
        loadRegistryUtxosTask loadingScopes contingencyScope =
            let
                registryAssetName =
                    SundaeTypes.registryTokenName

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
    case initLoading scripts form of
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
                        LoadingParams.encode
                        { key = "treasuryLoadingParams" }
                        loadingTreasury.loadingParams
                        |> ConcurrentTask.map (\_ -> LoadingParamsSaved)

                tasks =
                    saveTreasuryLoadingParamsTask
                        :: loadPragmaUtxoTask
                        :: loadRegistryUtxosTask loadingTreasury.scopes loadingTreasury.contingency
                        :: loadPermissionsRefScriptsUtxosTask
            in
            Ok ( loadingTreasury, OutMsg ctx.localStateUtxos tasks )

        Err error ->
            Err error


refreshUtxos : Context a -> Loaded -> ( Loading, OutMsg )
refreshUtxos ctx loadedTreasury =
    -- Remove all known UTxOs from the local state
    -- Reset treasury management into the loading state without treasury utxos
    -- Emit the task/command to load all treasury utxos
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
    ( loadingTreasury
    , OutMsg cleanedLocalState [ allTreasuriesUtxosTask ]
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



-- Tasks


type TaskCompleted
    = LoadingParamsSaved
    | LoadedPragmaUtxo ( OutputReference, Output )
    | LoadedRegistryUtxos ( Scopes ( OutputReference, Output ), ( OutputReference, Output ) )
    | LoadedTreasuriesUtxos ( Scopes (Utxo.RefDict Output), Utxo.RefDict Output )
    | LoadedTreasuryRefScript (Bytes CredentialHash) (Maybe ( OutputReference, Output ))
    | LoadedPermissionsRefScript (Bytes CredentialHash) (Maybe ( OutputReference, Output ))


updateWithCompletedTask : Context a -> Scripts -> TaskCompleted -> Loading -> Result String ( Loading, OutMsg )
updateWithCompletedTask ctx scripts taskCompleted ({ scopes, contingency } as loadingTreasury) =
    let
        noOutMsg =
            OutMsg ctx.localStateUtxos []
    in
    case taskCompleted of
        LoadingParamsSaved ->
            Ok ( loadingTreasury, noOutMsg )

        LoadedPragmaUtxo ( ref, output ) ->
            case setPragmaUtxo scripts.sundaeTreasury ref output loadingTreasury of
                Ok updatedLoadingTreasury ->
                    -- Also create a task to retrieve the scopes treasuries UTxOs
                    -- now that we have the applied sundae treasury scripts for each scope
                    let
                        allTreasuriesUtxosTask : ConcurrentTask String (BytesMap CredentialHash (Utxo.RefDict Output))
                        allTreasuriesUtxosTask =
                            -- The list order is the same as the order of the scopes
                            Api.retrieveUtxosWithPaymentCreds { db = ctx.db } ctx.networkId allTreasuriesScriptHashes

                        allTreasuriesScriptHashes =
                            List.filterMap extractAppliedTreasuryScriptHash loadingScopes

                        -- The list order is the same as the order of the scopes
                        loadingScopes =
                            Scopes.toList updatedLoadingTreasury.scopes ++ [ updatedLoadingTreasury.contingency ]

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
                    Ok
                        ( updatedLoadingTreasury
                        , OutMsg ctx.localStateUtxos (loadTreasuriesUtxosTask :: allTreasuryRefScriptsUtxosTask)
                        )

                Err error ->
                    Err error

        LoadedRegistryUtxos ( registryUtxos, contingencyUtxo ) ->
            Ok
                ( { loadingTreasury
                    | scopes = setRegistryUtxos registryUtxos scopes
                    , contingency = { contingency | registryUtxo = RemoteData.Success contingencyUtxo }
                  }
                , noOutMsg
                )

        LoadedTreasuriesUtxos ( treasuriesUtxos, contingencyUtxos ) ->
            Ok
                ( { loadingTreasury
                    | scopes = setTreasuryUtxos treasuriesUtxos scopes
                    , contingency = { contingency | treasuryUtxos = RemoteData.Success contingencyUtxos }
                  }
                , noOutMsg
                )

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
            Ok
                ( { loadingTreasury
                    | scopes = Scopes.map updateTreasuryRefScriptIfGoodScope scopes
                    , contingency = updateTreasuryRefScriptIfGoodScope contingency
                  }
                , noOutMsg
                )

        LoadedPermissionsRefScript scriptHash maybeUtxo ->
            let
                updatePermissionsRefScriptIfGoodScope scope =
                    if scriptHash == Tuple.first scope.permissionsScriptApplied then
                        { scope | permissionsScriptPublished = RemoteData.Success maybeUtxo }

                    else
                        scope
            in
            Ok
                ( { loadingTreasury
                    | scopes = Scopes.map updatePermissionsRefScriptIfGoodScope scopes
                    , contingency = updatePermissionsRefScriptIfGoodScope contingency
                  }
                , noOutMsg
                )


{-| Extract the scopes owner config from the root pragma utxo.
Also apply the Sundae treasuy contracts with the now known paramaters.
-}
setPragmaUtxo : PlutusScript -> OutputReference -> Output -> Loading -> Result String Loading
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

        treasuryConfig : SundaeTypes.TreasuryConfiguration
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
    Treasury.Sundae.initializeScript treasuryConfig unappliedScript
        |> Result.Extra.unpack RemoteData.Failure
            (\applied -> RemoteData.Success ( Script.hash <| Script.Plutus applied, applied ))


setRegistryUtxos : Scopes ( OutputReference, Output ) -> Scopes LoadingScope -> Scopes LoadingScope
setRegistryUtxos registryUtxos scopes =
    Scopes.map2 (\utxo loadingScope -> { loadingScope | registryUtxo = RemoteData.Success utxo }) registryUtxos scopes


setTreasuryUtxos : Scopes (Utxo.RefDict Output) -> Scopes LoadingScope -> Scopes LoadingScope
setTreasuryUtxos treasuryUtxos scopes =
    Scopes.map2 (\utxos loadingScope -> { loadingScope | treasuryUtxos = RemoteData.Success utxos }) treasuryUtxos scopes



-- Upgrade from Loading to Loaded


upgradeIfTreasuryLoadingFinished : Utxo.RefDict Output -> Loading -> Maybe ( Loaded, Utxo.RefDict Output )
upgradeIfTreasuryLoadingFinished localStateUtxos { rootUtxo, loadingParams, scopes, contingency } =
    case ( rootUtxo, upgradeScopesIfLoadingFinished scopes, upgradeScope contingency ) of
        ( RemoteData.Success ( ref, output ), Just loadedScopes, Just contingencyScope ) ->
            -- Upgrade the treasury management
            -- AND the local state utxos
            Just
                ( { rootUtxo = ( ref, output )
                  , loadingParams = loadingParams
                  , scopes = loadedScopes
                  , contingency = contingencyScope
                  }
                , addLoadedUtxos ( ref, output ) loadedScopes contingencyScope localStateUtxos
                )

        _ ->
            Nothing


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



--


doubleCheckTreasuryScriptsHashes : Loaded -> Result String ()
doubleCheckTreasuryScriptsHashes loadedTreasury =
    -- Make sure the scripts hashes obtained from applying the scripts
    -- match the ones obtained from the registry datums
    (Scopes.toList loadedTreasury.scopes ++ [ loadedTreasury.contingency ])
        |> Result.Extra.combineMap doublecheckTreasuryScriptHash
        |> Result.map (\_ -> ())


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
                        |> Maybe.andThen SundaeTypes.registryFromData
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



-- VIEW ##############################################################


view : Loading -> Html msg
view { rootUtxo, loadingParams, scopes, contingency } =
    div []
        [ Html.p [] [ text "Loading treasury ... ", spinner ]
        , viewLoadingRootUtxo rootUtxo
        , LoadingParams.view loadingParams
        , viewLoadingScope "ledger" scopes.ledger
        , viewLoadingScope "consensus" scopes.consensus
        , viewLoadingScope "mercenaries" scopes.mercenaries
        , viewLoadingScope "marketing" scopes.marketing
        , viewLoadingScope "contingency" contingency
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


viewMaybeOwner : Maybe MultisigScript -> Html msg
viewMaybeOwner maybeOwner =
    case maybeOwner of
        Nothing ->
            Html.p [] [ text <| "Owner: loading ... ", spinner ]

        Just owner ->
            viewOwner owner


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
