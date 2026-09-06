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
            |> set #config (sourceConfig (param @Text "tokenEnv") (checkbox "writeBack") (param @Text "cmdbSpace") (param @Text "jiraProject") historyDaysParam)
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
            |> set #config (sourceConfig (param @Text "tokenEnv") (checkbox "writeBack") (param @Text "cmdbSpace") (param @Text "jiraProject") historyDaysParam)
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

-- | Credentials stay env-var references ({"tokenEnv":"GRAFANA_TOKEN"}), never
-- raw tokens in the row. Integration toggles (milestone_3.md §8): writeBack,
-- cmdbSpace, jiraProject.
sourceConfig :: Text -> Bool -> Text -> Text -> Maybe Int -> Aeson.Value
sourceConfig tokenEnv writeBack cmdbSpace jiraProject historyDays = object $
    [ "writeBack" .= writeBack ]
    ++ [ "tokenEnv" .= tokenEnv | tokenEnv /= "" ]
    ++ [ "cmdbSpace" .= cmdbSpace | cmdbSpace /= "" ]
    ++ [ "jiraProject" .= jiraProject | jiraProject /= "" ]
    ++ [ "initialHistoryDays" .= days | Just days <- [historyDays] ]

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
