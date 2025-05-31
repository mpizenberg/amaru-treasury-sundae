port module Main exposing (main)

import Api exposing (ProtocolParams)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Data as Data exposing (Data)
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.TxIntent as TxIntent exposing (TxIntent, TxOtherInfo)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import ConcurrentTask
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


pragmaScriptHash : Bytes CredentialHash
pragmaScriptHash =
    Bytes.fromHexUnchecked "5bc659e149349b7d1d23493c7b7276a2ac83ad07c4249c125d3b1f49"


pragmaScript : PlutusScript
pragmaScript =
    Script.plutusScriptFromBytes PlutusV3 <|
        Bytes.fromHexUnchecked "59082a0101003229800aba2aba1aba0aab9faab9eaab9dab9a4888888966003300130033754011230073008001911919800800801912cc00400629422b30013003300a0018a518998010011805800a00a4021370e90004dc3a4009370e9001488c8cc00400400c88cc00c004c00800a4600e601060100032232330010010032259800800c5300103d87a8000899192cc004cdc8802800c56600266e3c014006266e9520003300b30090024bd7045300103d87a8000401d133004004300d003401c6eb8c01c004c0280050082444444445300130100099807804ca60026eb0c040c044c044c044c044c044c044c044c044c034dd5000cc040c044c044c044c044c044c044c044c034dd5000cdd5980818089808980898089808980898069baa0014888c8cc0040053014ad87a9f9fd8799f581c7095faf3d48d582fbae8b3f2e726670d7a35e2400c783d992bbdeffbffd8799f581c70b46e985fa50328fcb3d80594b6c5c54974a08d2c766a47570bfa36ffffff002232598009806800c4c8c8cc004004020896600200314a115980099b8f375c603200200714a3133002002301a001405080b8dd7180b18099baa0028acc004c02c006264660020026eb0c05cc050dd5001912cc00400629462b3001330050053018001899801001180c800c528202640591598009806000c4cc038dd6180b18099baa0022330040040018acc004cdc3a400c003132337126eb4c05c004c8cc004004dd6180c180c801112cc0040062900044cc896600266010010005133700002900144005016180c80099801001180d000a02e3013375400515980099b874802000626644b3001300d30143754003132598009807180a9baa3012301637546024602c6ea8022266e2400400e266e2000400d0141bad30183015375400314a08098dd6980b18099baa002301630133754601e60266ea80162b30013370e9005000c4cc8966002601a60286ea8006264b3001300e301537546024602c6ea8c064c058dd500444cdc4801800c4cdc4001800a028375a6030602a6ea800629410131bad301630133754004602c60266ea8c058c04cdd5002c4c8c8cc004004018896600200314a115980099baf003301530190018a51899801001180d000a028405c66e9520023301530163013375400497ae040448089011202240448088c044dd5000a44464b3001300a001899192cc004c05800a00916404c6eb8c050004c040dd5001c566002601000313259800980a800c4cc020dd6180a000912cc00400a00b19800803cc05800a26002602e004803901445901218081baa0038acc004c024006264b300130150018998041bac30140012259800801401633001007980b00144c004c05c00900720288b20243010375400715980099b87480180062646644b300130170018998051bac30160012259800801401e33001009980c00144c004c064009009202c8b2028375a6028002602a00260206ea800e2b30013370e9004000c4c8c966002602c0050048b2026375a602800260206ea800e2b30013370e9005000c4c8c966002602c0050048b2026375a602800260206ea800e2b30013370e9006000c4c8c966002602c0050048b2026375c602800260206ea800e2c807100e201c4038807100e201c300e37540052223259800980418079baa0018a40011375a602660206ea800500e192cc004c020c03cdd5000c5300103d87a8000899198008009bab30143011375400444b30010018a6103d87a8000899192cc004cdc8803000c56600266e3c018006266e9520003301630140024bd7045300103d87a80004049133004004301800340486eb8c048004c054005013201c33004003002488888cc8a600260246ea800a4b3001300d30133754003132598009807980a1baa0018991919194c004c0700066038007301c002488966002604000913300a301f00713300a00213300a00113300a00314a3164074301c001301b001301a0013015375400316404c602e60286ea80062c8092602c00e9112cc004c03800e2646644b3001300b00c898029807980c9baa3232330010013758602260366ea80388966002003168992cc0056600266ebcc080c074dd51810180e9baa0010048acc004c054c070dd500244c05660026eacc064c074dd5000cdd71810180e9baa004a450c616d6172752073636f70657300403516406d14a080da20031330030033021002406c603e00280e8c070c064dd5180e180c9baa32330010013758603a60346ea80348966002003168992cc004cdd7980f980e1baa0010058980c180e1baa00189980180198100012034301e001407116405c603400260346036002602c6ea80162b300130100038cc004dd7180c980b1baa0059180d180d980d980d980d800c88c966002602260306ea8006297adef6c6089bab301c3019375400280b8cc03400800522259800acc004cc050dd6180e180c9baa00c23375e603a60346ea80040822b30013375e6ea0ca60020033758602060346ea80369000200222259800801440063300100398100014c96600266ebcc080c074dd51810180e9baa0013374a90011980f9ba90074bd70456600260126026603a6ea8006266e0000a60026eacc064c074dd5000c01e9110c616d6172752073636f70657300403516406d1002406c603e004801901d2601010100899baf374c660026eacc008c064dd5006001a6010fa14c616d6172752073636f70657301008a50405d14a080ba29462b3001300b00c899baf374c660026eacc008c064dd5006001a610fa14c616d6172752073636f70657320008a50405c80b91300830193016375403680a10140c050c054014c0080088a4d1365640044c127d8799f5820a8fdcc3912943ee940c0d32db88186337b8346346079e5f945b534a791c43e2601ff0001"


type alias Flags =
    { url : String
    , db : JD.Value
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
    , scripts : Scripts
    , treasuryManagement : TreasuryManagement
    , error : Maybe String
    }


initialModel : JD.Value -> Scripts -> Model
initialModel db scripts =
    { taskPool = ConcurrentTask.pool
    , db = db
    , page = Home
    , networkId = Testnet
    , protocolParams = Api.defaultProtocolParams
    , discoveredWallets = []
    , connectedWallet = Nothing
    , localStateUtxos = Utxo.emptyRefDict
    , scripts = scripts
    , treasuryManagement = TreasuryUnspecified
    , error = Nothing
    }


type Page
    = Home


type alias Scripts =
    { sundaeTreasury : PlutusScript
    , scopeOwner : PlutusScript
    , pragma : PlutusScript
    }


type TreasuryManagement
    = TreasuryUnspecified
    | TreasuryLoading LoadingTreasury
    | TreasuryFullyLoaded LoadedTreasury


type alias LoadingTreasury =
    { rootUtxo : RemoteData String ( OutputReference, Output )
    , scopes : LoadingScopes
    }


type alias LoadingScopes =
    { ledger : LoadingScope
    , consensus : LoadingScope
    , merceneries : LoadingScope
    , marketing : LoadingScope
    }


startLoadingTreasury : LoadingTreasury
startLoadingTreasury =
    { rootUtxo = RemoteData.Loading
    , scopes =
        { ledger = startLoadingScope
        , consensus = startLoadingScope
        , merceneries = startLoadingScope
        , marketing = startLoadingScope
        }
    }


type alias LoadingScope =
    { owner : Maybe MultisigScript
    , registryUtxo : RemoteData String ( OutputReference, Output )
    , scopeUtxos : RemoteData String (Utxo.RefDict Output)
    }


startLoadingScope : LoadingScope
startLoadingScope =
    { owner = Nothing
    , registryUtxo = RemoteData.Loading
    , scopeUtxos = RemoteData.Loading
    }


failedBeforeLoadingScope : String -> LoadingScope
failedBeforeLoadingScope failure =
    { owner = Nothing
    , registryUtxo = RemoteData.Failure failure
    , scopeUtxos = RemoteData.Failure failure
    }


initLoadingScopeWithOwner : MultisigScript -> LoadingScope
initLoadingScopeWithOwner owner =
    { startLoadingScope | owner = Just owner }


type alias LoadedTreasury =
    { rootUtxo : ( OutputReference, Output )
    , scopes : Scopes
    }


type alias Scopes =
    { ledger : Scope
    , consensus : Scope
    , merceneries : Scope
    , marketing : Scope
    }


type alias Scope =
    { owner : MultisigScript
    , ownerScript : PlutusScript
    , sundaeTreasuryScript : PlutusScript
    , registryUtxo : ( OutputReference, Output )

    -- TODO: make sure they are updated after every Tx
    , utxos : Utxo.RefDict Output
    }


init : Flags -> ( Model, Cmd Msg )
init { db, blueprints } =
    let
        decodedBlueprints : List ScriptBlueprint
        decodedBlueprints =
            List.map (JD.decodeValue blueprintDecoder) blueprints
                |> Result.Extra.combine
                |> Result.withDefault []
                |> List.concat

        sundaeTreasuryBlueprint =
            List.Extra.find (\{ name } -> name == "treasury.treasury.spend") decodedBlueprints

        ( scripts, error ) =
            case sundaeTreasuryBlueprint of
                Nothing ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , scopeOwner = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , pragma = pragmaScript
                      }
                    , Just "Failed the retrieve the blueprint of the Sundae Treasury script"
                    )

                Just blueprint ->
                    ( { sundaeTreasury = Script.plutusScriptFromBytes PlutusV3 blueprint.scriptBytes
                      , scopeOwner = Script.plutusScriptFromBytes PlutusV3 Bytes.empty
                      , pragma = pragmaScript
                      }
                    , Nothing
                    )

        model =
            initialModel db scripts

        -- Load Pragma UTxO
        ( updatedTaskPool, loadPragmaUtxoCmd ) =
            Api.retrieveAssetUtxo model.networkId pragmaScriptHash (Bytes.fromText "amaru scopes")
                |> ConcurrentTask.mapError Debug.toString
                |> ConcurrentTask.andThen (Api.retrieveOutput model.networkId)
                |> ConcurrentTask.map LoadedPragmaUtxo
                |> ConcurrentTask.attempt
                    { pool = model.taskPool
                    , send = sendTask
                    , onComplete = OnTaskComplete
                    }
    in
    ( { model
        | taskPool = updatedTaskPool
        , treasuryManagement = TreasuryLoading startLoadingTreasury
        , error = error
      }
    , Cmd.batch
        [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
        , Api.loadProtocolParams model.networkId GotNetworkParams
        , loadPragmaUtxoCmd
        ]
    )


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
                    ( { model | treasuryManagement = setPragmaUtxo ref output loadingTreasury }
                    , Cmd.none
                    )

                _ ->
                    ( Debug.todo "", Cmd.none )


setPragmaUtxo : OutputReference -> Output -> LoadingTreasury -> TreasuryManagement
setPragmaUtxo ref output loading =
    let
        scopes =
            case output.datumOption of
                Just (DatumValue { rawBytes }) ->
                    case Data.fromBytes rawBytes of
                        Nothing ->
                            scopesError "The PRAGMA UTxO datum does not contain valid Data."

                        Just data ->
                            initLoadingScopesFromData data

                _ ->
                    scopesError "The PRAGMA UTxO does not contain a valid datum"
    in
    TreasuryLoading
        { loading
            | rootUtxo = RemoteData.Success ( ref, output )
            , scopes = scopes
        }


scopesError : String -> LoadingScopes
scopesError error =
    { ledger = failedBeforeLoadingScope error
    , consensus = failedBeforeLoadingScope error
    , merceneries = failedBeforeLoadingScope error
    , marketing = failedBeforeLoadingScope error
    }


initLoadingScopesFromData : Data -> LoadingScopes
initLoadingScopesFromData data =
    case data of
        Data.Constr _ [ owner1, owner2, owner3, owner4 ] ->
            case ( ( MultisigScript.fromData owner1, MultisigScript.fromData owner2 ), ( MultisigScript.fromData owner3, MultisigScript.fromData owner4 ) ) of
                ( ( Just ms1, Just ms2 ), ( Just ms3, Just ms4 ) ) ->
                    { ledger = initLoadingScopeWithOwner ms1
                    , consensus = initLoadingScopeWithOwner ms2
                    , merceneries = initLoadingScopeWithOwner ms3
                    , marketing = initLoadingScopeWithOwner ms4
                    }

                ( ( Nothing, _ ), _ ) ->
                    scopesError "The first scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( ( _, Nothing ), _ ) ->
                    scopesError "The second scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( Nothing, _ ) ) ->
                    scopesError "The third scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

                ( _, ( _, Nothing ) ) ->
                    scopesError "The fourth scope owner in the PRAGMA UTxO datum is not a valid aicone multisig"

        _ ->
            scopesError "The PRAGMA UTxO does not contain exactly 4 owners"



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
viewLoadingScope scopeName { owner, registryUtxo, scopeUtxos } =
    div [ HA.style "border" "1px solid black" ]
        [ Html.h4 [] [ text <| "Scope: " ++ scopeName ]
        , viewOwner owner
        , viewRegistryUtxo registryUtxo
        , viewScopeUtxos scopeUtxos
        ]


viewOwner : Maybe MultisigScript -> Html msg
viewOwner maybeOwner =
    case maybeOwner of
        Nothing ->
            Html.p [] [ text <| "Unknow owner" ]

        Just owner ->
            Html.p [] [ text <| "Owner: " ++ Debug.toString owner ]


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
