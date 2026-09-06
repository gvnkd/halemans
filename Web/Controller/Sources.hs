module Web.Controller.Sources where

import Web.Controller.Prelude
import Web.View.Sources.Index
import Web.View.Sources.New
import Web.View.Sources.Edit
import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Text.Read (readMaybe)
import qualified Application.Connector.Zabbix as Zabbix
import Application.Service.HostGroups (replaceHostGroupCache)
import Control.Exception (try, SomeException)
import System.Environment (lookupEnv)

instance Controller SourcesController where
    beforeAction = ensureIsUser

    action SourcesAction = do
        sources <- query @Source
            |> orderByAsc #name
            |> fetch
        canManage <- currentUserHasPrivilege "manage_sources"
        render IndexView { .. }

    action NewSourceAction = do
        requirePrivilege "manage_sources"
        render NewView

    action CreateSourceAction = do
        requirePrivilege "manage_sources"
        _ <- newRecord @Source
            |> set #type_ (param @Text "type")
            |> set #name (param @Text "name")
            |> set #baseUrl (param @Text "baseUrl")
            |> set #env (param @Text "env")
            |> set #pollIntervalSeconds (param @Int "pollIntervalSeconds")
            |> set #enabled True
            |> set #config (sourceConfig (param @Text "tokenEnv") (checkbox "writeBack") (param @Text "cmdbSpace") (param @Text "jiraProject") historyDaysParam (param @Text "hostGroupScope"))
            |> createRecord
        setSuccessMessage "Source created"
        redirectTo SourcesAction

    action EditSourceAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        render EditView
            { source
            , tokenEnv = tokenEnvOf source
            , writeBack = configBool "writeBack" source
            , cmdbSpace = configValue "cmdbSpace" source
            , jiraProject = configValue "jiraProject" source
            , initialHistoryDays = maybe "" tshow (configInt "initialHistoryDays" source)
            , hostGroupScope = configValue "hostGroupScope" source
            }

    action UpdateSourceAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        _ <- source
            |> set #type_ (param @Text "type")
            |> set #name (param @Text "name")
            |> set #baseUrl (param @Text "baseUrl")
            |> set #env (param @Text "env")
            |> set #pollIntervalSeconds (param @Int "pollIntervalSeconds")
            |> set #config (sourceConfig (param @Text "tokenEnv") (checkbox "writeBack") (param @Text "cmdbSpace") (param @Text "jiraProject") historyDaysParam (param @Text "hostGroupScope"))
            |> updateRecord
        setSuccessMessage "Source updated"
        redirectTo SourcesAction

    action ToggleSourceAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        _ <- source
            |> set #enabled (not source.enabled)
            |> updateRecord
        setSuccessMessage (if source.enabled then "Source disabled" else "Source enabled")
        redirectTo SourcesAction

    -- | Manual sync of the zabbix host group cache (zabbix_host_groups).
    -- Host groups are near-static, so the poller never calls hostgroup.get —
    -- an admin triggers this after group changes on the zabbix side.
    action SyncHostGroupsAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        if get #type_ source /= ("zabbix" :: Text)
            then setErrorMessage "Host group sync is only available for zabbix sources"
            else do
                result <- syncHostGroups source
                case result of
                    Left err -> setErrorMessage ("Host group sync failed: " <> err)
                    Right count -> setSuccessMessage ("Synced " <> tshow count <> " host groups")
        redirectTo SourcesAction

-- | Credentials stay env-var references ({"tokenEnv":"GRAFANA_TOKEN"}), never
-- raw tokens in the row. Integration toggles (milestone_3.md §8): writeBack,
-- cmdbSpace, jiraProject.
sourceConfig :: Text -> Bool -> Text -> Text -> Maybe Int -> Text -> Aeson.Value
sourceConfig tokenEnv writeBack cmdbSpace jiraProject historyDays scope = object $
    [ "writeBack" .= writeBack ]
    ++ [ "tokenEnv" .= tokenEnv | tokenEnv /= "" ]
    ++ [ "cmdbSpace" .= cmdbSpace | cmdbSpace /= "" ]
    ++ [ "jiraProject" .= jiraProject | jiraProject /= "" ]
    ++ [ "initialHistoryDays" .= days | Just days <- [historyDays] ]
    ++ [ "hostGroupScope" .= scope | scope == "teams" ]

-- | Empty/unparseable input omits the key (poller default applies).
historyDaysParam :: (?request :: Request, ?respond :: Respond) => Maybe Int
historyDaysParam = paramOrNothing @Text "initialHistoryDays" >>= (readMaybe . cs)

checkbox :: (?request :: Request, ?respond :: Respond) => ByteString -> Bool
checkbox name = isJust (paramOrNothing @Text name)

tokenEnvOf :: Source -> Text
tokenEnvOf source = configValue "tokenEnv" source

configValue :: Text -> Source -> Text
configValue key source = fromMaybe "" (parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? Key.fromText key Aeson..!= "")) source.config)

configBool :: Text -> Source -> Bool
configBool key source = fromMaybe False (parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? Key.fromText key Aeson..!= False)) source.config)

configInt :: Text -> Source -> Maybe Int
configInt key source = parseMaybe (Aeson.withObject "config" (\o -> o Aeson..: Key.fromText key)) source.config

-- | Replace the source's host group cache with a fresh hostgroup.get listing.
syncHostGroups :: (?modelContext :: ModelContext) => Source -> IO (Either Text Int)
syncHostGroups source = do
    let tokenEnv :: Maybe Text
        tokenEnv = parseMaybe (Aeson.withObject "source.config" (\o -> o Aeson..: "tokenEnv")) source.config
    token <- case tokenEnv of
        Just envVar -> fmap cs <$> lookupEnv (cs envVar)
        Nothing -> pure Nothing
    case token of
        Nothing -> pure (Left "token env var is not set")
        Just token -> do
            outcome <- try (Zabbix.hostGroupsGetAll source.baseUrl token)
            case outcome of
                Left err -> pure (Left (tshow (err :: SomeException)))
                Right (Left err) -> pure (Left err)
                Right (Right groups) -> do
                    count <- replaceHostGroupCache (get #id source) groups
                    pure (Right count)
