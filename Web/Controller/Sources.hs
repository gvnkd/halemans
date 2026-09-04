module Web.Controller.Sources where

import Web.Controller.Prelude
import Web.View.Sources.Index
import Web.View.Sources.New
import Web.View.Sources.Edit
import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson

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
            |> set #config (tokenConfig (param @Text "tokenEnv"))
            |> createRecord
        setSuccessMessage "Source created"
        redirectTo SourcesAction

    action EditSourceAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        render EditView { source, tokenEnv = tokenEnvOf source }

    action UpdateSourceAction { sourceId } = do
        requirePrivilege "manage_sources"
        source <- fetch sourceId
        _ <- source
            |> set #type_ (param @Text "type")
            |> set #name (param @Text "name")
            |> set #baseUrl (param @Text "baseUrl")
            |> set #env (param @Text "env")
            |> set #pollIntervalSeconds (param @Int "pollIntervalSeconds")
            |> set #config (tokenConfig (param @Text "tokenEnv"))
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
-- raw tokens in the row.
tokenConfig :: Text -> Aeson.Value
tokenConfig tokenEnv
    | tokenEnv == "" = object []
    | otherwise = object ["tokenEnv" .= tokenEnv]

tokenEnvOf :: Source -> Text
tokenEnvOf source = fromMaybe "" (parseMaybe (Aeson.withObject "config" (\o -> o Aeson..:? "tokenEnv" Aeson..!= "")) source.config)
