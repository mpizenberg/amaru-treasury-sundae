port module Main exposing (main)

import Api exposing (ProtocolParams)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.TxIntent as TxIntent exposing (TxIntent, TxOtherInfo)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Cbor.Encode
import ConcurrentTask exposing (ConcurrentTask)
import Dict
import Dict.Any
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra
import MultisigScript exposing (MultisigScript)
import Natural as N
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData)
import Result.Extra
import Treasury exposing (SpendConfig)
import Types


type alias Flags =
    { url : String
    , db : JD.Value
    , pragmaScriptHash : String
    , registriesSeedUtxo : { txId : String, outputIndex : Int }
    , blueprints : List JD.Value
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
    | UrlChanged String
    | WalletMsg JD.Value
    | ConnectButtonClicked { id : String }
    | GotNetworkParams (Result Http.Error ProtocolParams)
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type TaskCompleted
    = LoadedPragmaUtxo ( OutputReference, Output )



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
    , pragmaScriptHash : Bytes CredentialHash
    , registriesSeedUtxo : OutputReference
    , scripts : Scripts
    , treasuryManagement : TreasuryManagement
    , error : Maybe String
    }


initialModel : JD.Value -> String -> ( String, Int ) -> Scripts -> Model
initialModel db pragmaScriptHash ( txId, outputIndex ) scripts =
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , pragmaScriptHash = Bytes.fromHexUnchecked pragmaScriptHash
    , registriesSeedUtxo = OutputReference (Bytes.fromHexUnchecked txId) outputIndex
    , scripts = scripts
    , treasuryManagement = TreasuryUnspecified
    , error = Nothing
    }


type Page
    = Home


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , registryTrap : PlutusScript
    , scopePermissions : PlutusScript
    }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


type alias LoadingTreasury =
    { rootUtxo : RemoteData String ( OutputReference, Output )
    , scopes : Scopes LoadingScope
    }


initLoadingTreasury : PlutusScript -> Bytes CredentialHash -> PlutusScript -> OutputReference -> Result String LoadingTreasury
initLoadingTreasury unappliedScopePermissionScript pragmaScriptHash unappliedRegistryTrapScript registriesSeedUtxo =
    let
        initLoadingScopeWithIndex index =
            initLoadingScope
                unappliedScopePermissionScript
                pragmaScriptHash
                unappliedRegistryTrapScript
                registriesSeedUtxo
                index

        loadingTreasury ledger consensus merceneries marketing =
            { rootUtxo = RemoteData.Loading
            , scopes = Scopes ledger consensus merceneries marketing
            }
    in
    Result.map4 loadingTreasury
        (initLoadingScopeWithIndex 0)
        (initLoadingScopeWithIndex 1)
        (initLoadingScopeWithIndex 2)
        (initLoadingScopeWithIndex 3)


type alias LoadingScope =
    { owner : Maybe MultisigScript
    , permissionsScriptApplied : ( Bytes CredentialHash, PlutusScript )
    , sundaeTreasuryScriptApplied : RemoteData String ( Bytes CredentialHash, PlutusScript )
    , registryNftPolicyId : Bytes PolicyId
    , registryUtxo : RemoteData String ( OutputReference, Output )
    , scopeUtxos : RemoteData String (Utxo.RefDict Output)
    }


initLoadingScope : PlutusScript -> Bytes CredentialHash -> PlutusScript -> OutputReference -> Int -> Result String LoadingScope
initLoadingScope unappliedScopePermissionScript pragmaScriptHash unappliedRegistryTrapScript registriesSeedUtxo scopeIndex =
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
            , scopeUtxos = RemoteData.Loading
            }
    in
    Result.map2 loadingScope
        permissionsScriptResult
        registryScriptResult


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , scopes : Scopes Scope
    }


type alias Scopes a =
    { ledger : a
    , consensus : a
    , merceneries : a
    , marketing : a
    }


type alias Scope =
    { owner : MultisigScript
    , ownerScript : PlutusScript
    , sundaeTreasuryScript : PlutusScript
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , utxos : Utxo.RefDict Output
    }


type alias LoadedUtxos =
    { rootUtxo : ( OutputReference, Output )
    , registryUtxos : Scopes ( OutputReference, Output )
    , scopeUtxos : Scopes (List ( OutputReference, Output ))
    }


init : Flags -> ( Model, Cmd Msg )
init { db, pragmaScriptHash, registriesSeedUtxo, blueprints } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        registryBlueprint =
            List.Extra.find (\{ name } -> name == "traps.treasury_registry.spend") decodedBlueprints

        amaruTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "permissions.permissions.withdraw") decodedBlueprints

        ( scripts, blueprintError ) =
            case ( sundaeTreasuryBlueprint, registryBlueprint, amaruTreasuryBlueprint ) of
                ( Just treasury, Just registry, Just amaru ) ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 treasury.scriptBytes
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 registry.scriptBytes
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 amaru.scriptBytes
                      }
                    , Nothing
                    )

                _ ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , registryTrap = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopePermissions = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      }
                    , Just "Failed the retrieve some of the Plutus blueprints. Did you forget to build the aiken code?"
                    )

        model =
            initialModel db pragmaScriptHash ( registriesSeedUtxo.txId, registriesSeedUtxo.outputIndex ) scripts

        -- Load Pragma UTxO
        ( updatedTaskPool, loadPragmaUtxoCmd ) =
            Api.retrieveAssetUtxo model.networkId model.pragmaScriptHash (Bytes.fromText "amaru scopes")
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
                model.pragmaScriptHash
                model.scripts.registryTrap
                model.registriesSeedUtxo

        loadAllUtxos : Scopes LoadingScope -> ConcurrentTask String LoadedUtxos
        loadAllUtxos loadingScopes =
            let
                pragmaRootAsset =
                    ( model.pragmaScriptHash, Bytes.fromText "amaru scopes" )

                registryAssets =
                    Scopes
                        ( loadingScopes.ledger.registryNftPolicyId, Bytes.fromText "ledger" )
                        ( loadingScopes.consensus.registryNftPolicyId, Bytes.fromText "consensus" )
                        ( loadingScopes.merceneries.registryNftPolicyId, Bytes.fromText "merceneries" )
                        ( loadingScopes.marketing.registryNftPolicyId, Bytes.fromText "marketing" )

                -- Query assets utxos
                assetsUtxosTask : ConcurrentTask String (MultiAsset (Utxo.RefDict Output))
                assetsUtxosTask =
                    Api.retrieveAssetsUtxos model.networkId
                        [ pragmaRootAsset
                        , registryAssets.ledger
                        , registryAssets.consensus
                        , registryAssets.merceneries
                        , registryAssets.marketing
                        ]

                -- TODO: Query scope utxos
                --
                extractLoadedUtxos : MultiAsset (Utxo.RefDict Output) -> LoadedUtxos
                extractLoadedUtxos assetsUtxos =
                    Debug.todo ""
            in
            ConcurrentTask.map extractLoadedUtxos assetsUtxosTask
    in
    case ( blueprintError, loadingTreasuryResult ) of
        ( Nothing, Ok loadingTreasury ) ->
            -- TODO: make requests to retrieve all relevant UTxOs:
            --  - pragma root utxo
            --  - all 4 registry utxos
            --  - all scope utxos
            ( { model
                | taskPool = updatedTaskPool
                , treasuryManagement = TreasuryLoading loadingTreasury
              }
            , Cmd.batch
                [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
                , Api.loadProtocolParams model.networkId GotNetworkParams
                , loadPragmaUtxoCmd
                ]
            )

        ( Just error, _ ) ->
            ( { model | error = Just error }, Cmd.none )

        ( _, Err error ) ->
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

        UrlChanged _ ->
            ( Debug.todo "", Debug.todo "" )

        WalletMsg value ->
            handleWalletMsg value model

        ConnectButtonClicked { id } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [], watchInterval = Just 3 })) )

        GotNetworkParams (Err httpError) ->
            ( { model | error = Just <| Debug.toString httpError }, Cmd.none )

        GotNetworkParams (Ok params) ->
            ( { model | protocolParams = params }, Cmd.none )

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

        _ ->
            ( model, Cmd.none )



-- Merge UTxOs


{-| Merge all UTxOs from a given scope.

REMARK: you also need to add a `TxIntent.TxReferenceInput rootUtxoRef`

-}
mergeUtxos : NetworkId -> Scope -> List (Bytes CredentialHash) -> List TxIntent
mergeUtxos networkId scope requiredSigners =
    let
        utxos =
            Dict.Any.toList scope.utxos

        treasuryScriptBytes =
            Script.cborWrappedBytes scope.sundaeTreasuryScript

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
                                ( Script.plutusVersion scope.ownerScript
                                , Witness.ByValue <| Script.cborWrappedBytes scope.ownerScript
                                )
                            , redeemerData = \_ -> Debug.todo "Standard Owner Redeemer"
                            , requiredSigners = requiredSigners
                            }
              }
            ]

        ownerScriptHash =
            Script.hash <| Script.Plutus scope.ownerScript

        receivers value =
            [ TxIntent.SendTo scopeTreasuryAddress value ]

        scopeTreasuryAddress =
            case utxos of
                ( _, { address } ) :: _ ->
                    address

                _ ->
                    Debug.todo "impossible to have utxos = []"
    in
    Treasury.reorganize treasuryScriptBytes requiredSigners requiredWithdrawals utxos receivers



-- Disburse


{-| Disburse funds from one UTxO in the given scope.

REMARK: you also need to add a `TxIntent.TxReferenceInput rootUtxoRef`

-}
disburse : NetworkId -> Scope -> List (Bytes CredentialHash) -> OutputReference -> (Value -> List TxIntent) -> Value -> Result String (List TxIntent)
disburse networkId scope requiredSigners utxoRef receivers value =
    case Dict.Any.get utxoRef scope.utxos of
        Nothing ->
            Err <| "The selected UTxO isn’t in the known list of UTxOs for this scope: " ++ Debug.toString utxoRef

        Just spentOutput ->
            let
                spendConfig : SpendConfig
                spendConfig =
                    { treasuryScriptBytes = Script.cborWrappedBytes scope.sundaeTreasuryScript
                    , requiredSigners = requiredSigners
                    , requiredWithdrawals = requiredWithdrawals
                    , spentInputRef = utxoRef
                    , spentOutput = spentOutput
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
                                        ( Script.plutusVersion scope.ownerScript
                                        , Witness.ByValue <| Script.cborWrappedBytes scope.ownerScript
                                        )
                                    , redeemerData = \_ -> Debug.todo "Standard/Swap Owner Redeemer"
                                    , requiredSigners = requiredSigners
                                    }
                      }
                    ]

                ownerScriptHash =
                    Script.hash <| Script.Plutus scope.ownerScript

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
                    ( setPragmaUtxo model.scripts.sundaeTreasury ref output loadingTreasury
                        |> Result.map (\treasuryManagement -> { model | treasuryManagement = treasuryManagement })
                        |> Result.Extra.extract (\error -> { model | error = Just error })
                      -- TODO: task to retrieve the scope UTxOs
                      -- now that we have the applied sundae treasury scripts for each scope
                    , Cmd.none
                    )

                _ ->
                    ( Debug.todo "", Cmd.none )


{-| Extract the scopes owner config from the root pragma utxo.
Also apply the Sundae treasuy contracts with the now known paramaters.
-}
setPragmaUtxo : PlutusScript -> OutputReference -> Output -> LoadingTreasury -> Result String TreasuryManagement
setPragmaUtxo unappliedSundaeTreasuryScript ref output loading =
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

        treasuryLoading okScopes =
            TreasuryLoading
                { loading
                    | rootUtxo = RemoteData.Success ( ref, output )
                    , scopes = okScopes
                }
    in
    Result.map treasuryLoading scopes


updateLoadingScopesFromData : PlutusScript -> Data -> Scopes LoadingScope -> Result String (Scopes LoadingScope)
updateLoadingScopesFromData unappliedSundaeTreasuryScript data { ledger, consensus, merceneries, marketing } =
    case data of
        Data.Constr _ [ owner1, owner2, owner3, owner4 ] ->
            case ( ( MultisigScript.fromData owner1, MultisigScript.fromData owner2 ), ( MultisigScript.fromData owner3, MultisigScript.fromData owner4 ) ) of
                ( ( Just ms1, Just ms2 ), ( Just ms3, Just ms4 ) ) ->
                    let
                        updateOwner scope owner =
                            { scope
                                | owner = Just owner
                                , sundaeTreasuryScriptApplied = applySundaeTreasuryScript unappliedSundaeTreasuryScript scope
                            }
                    in
                    Ok
                        { ledger = updateOwner ledger ms1
                        , consensus = updateOwner consensus ms2
                        , merceneries = updateOwner merceneries ms3
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
                { reorganize = Debug.log "multisig" multisig
                , sweep = multisig
                , fund = MultisigScript.AnyOf []
                , disburse = multisig
                , unregister = multisig
                }

            -- TODO: retrieve expiration from aiken build outputs
            , expiration = N.fromSafeInt 1767182399000
            , payoutUpperbound = N.zero
            }
    in
    Uplc.applyParamsToScript [ Types.treasuryConfigToData treasuryConfig ] unappliedScript
        |> Result.Extra.unpack RemoteData.Failure
            (\applied -> RemoteData.Success ( Script.hash <| Script.Plutus applied, applied ))



-- VIEW ##############################################################


view : Model -> Html Msg
view model =
    div []
        [ viewWalletSection model
        , viewTreasurySection model.treasuryManagement
        , viewError model.error
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
            div [] [ walletIcon w, text (walletDescription w), connectButton w ]
    in
    div [] (List.map walletRow wallets)


viewConnectedWallet : Cip30.Wallet -> Html Msg
viewConnectedWallet wallet =
    div []
        [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
        , div [] [ text <| "Address: " ++ (Address.toBech32 <| Cip30.walletChangeAddress wallet) ]
        ]



-- Treasury Section


viewTreasurySection : TreasuryManagement -> Html Msg
viewTreasurySection treasuryManagement =
    case treasuryManagement of
        TreasuryUnspecified ->
            div [] [ text "Treasury unspecified yet" ]

        TreasuryLoading { rootUtxo, scopes } ->
            div []
                [ Html.p [] [ text "Loading treasury ..." ]
                , viewLoadingRootUtxo rootUtxo
                , viewLoadingScope "ledger" scopes.ledger
                , viewLoadingScope "consensus" scopes.consensus
                , viewLoadingScope "merceneries" scopes.merceneries
                , viewLoadingScope "marketing" scopes.marketing
                ]

        TreasuryFullyLoaded _ ->
            div [] [ text "Treasury fully loaded" ]


viewLoadingRootUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewLoadingRootUtxo rootUtxo =
    case rootUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text "PRAGMA root UTxO not asked" ]

        RemoteData.Loading ->
            Html.p [] [ text "PRAGMA root UTxO loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "PRAGMA root UTxO failed to load: " ++ error ]

        RemoteData.Success _ ->
            Html.p [] [ text "PRAGMA root UTxO loaded" ]


viewLoadingScope : String -> LoadingScope -> Html msg
viewLoadingScope scopeName { owner, permissionsScriptApplied, sundaeTreasuryScriptApplied, registryNftPolicyId, registryUtxo, scopeUtxos } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewPermissionsScript permissionsScriptApplied
        , viewTreasuryScript sundaeTreasuryScriptApplied
        , viewRegistryNftPolicyId registryNftPolicyId
        , viewRegistryUtxo registryUtxo
        , viewScopeUtxos scopeUtxos
        ]


viewOwner : Maybe MultisigScript -> Html msg
viewOwner maybeOwner =
    case maybeOwner of
        Nothing ->
            Html.p [] [ text <| "Owner: loading ..." ]

        Just owner ->
            Html.p [] [ text <| "Owner: " ++ Debug.toString owner ]


viewPermissionsScript : ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewPermissionsScript ( hash, _ ) =
    Html.p [] [ text <| "Fully applied permissions script hash: " ++ Bytes.toHex hash ]


viewTreasuryScript : RemoteData String ( Bytes CredentialHash, PlutusScript ) -> Html msg
viewTreasuryScript remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Sundae treasury script not asked" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Sundae treasury script loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Sundae treasury script failed to load: " ++ error ]

        RemoteData.Success ( hash, _ ) ->
            Html.p [] [ text <| "Fully applied Sundae treasury script hash: " ++ Bytes.toHex hash ]


viewRegistryNftPolicyId : Bytes PolicyId -> Html msg
viewRegistryNftPolicyId policyId =
    Html.p [] [ text <| "Registry trap policy ID: " ++ Bytes.toHex policyId ]


viewRegistryUtxo : RemoteData String ( OutputReference, Output ) -> Html msg
viewRegistryUtxo registryUtxo =
    case registryUtxo of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Registry UTxO not asked" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Registry UTxO loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Registry UTxO failed to load: " ++ error ]

        RemoteData.Success _ ->
            Html.p [] [ text <| "Registry UTxO loaded" ]


viewScopeUtxos : RemoteData String (Utxo.RefDict Output) -> Html msg
viewScopeUtxos loadingUtxos =
    case loadingUtxos of
        RemoteData.NotAsked ->
            Html.p [] [ text <| "Scope UTxOs not asked" ]

        RemoteData.Loading ->
            Html.p [] [ text <| "Scope UTxOs loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "Scope UTxOs failed to load: " ++ error ]

        RemoteData.Success _ ->
            Html.p [] [ text <| "Scope UTxOs loaded" ]
