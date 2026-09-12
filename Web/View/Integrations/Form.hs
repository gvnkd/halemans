module Web.View.Integrations.Form (jiraConfigFormFields, cmdbConfigFormFields) where

import Application.Helper.Json (stringList)
import qualified Data.Text as Text
import Web.View.Prelude

-- Shared new/edit fields for jira_configs/cmdb_configs (milestone 10).
-- projects/spaces are JSONB string arrays on the row, edited as
-- comma-separated text; empty = no scope clause.

jiraConfigFormFields :: Maybe JiraConfig -> Html
jiraConfigFormFields config =
    [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={field (.name)} data-testid="jira-config-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Base URL</label>
        <input name="baseUrl" type="text" class="form-control" value={field (.baseUrl)} data-testid="jira-config-base-url" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Token env var</label>
        <input name="tokenEnv" type="text" class="form-control" value={field (.tokenEnv)} data-testid="jira-config-token-env" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">REST API version</label>
        <select name="apiVersion" class="form-select" data-testid="jira-config-api-version">
            <option value="3" selected={versionIs "3"}>3 (Jira Cloud)</option>
            <option value="2" selected={versionIs "2"}>2 (Jira Server / Data Center)</option>
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">Projects (comma-separated; empty = search all)</label>
        <input name="projects" type="text" class="form-control" value={projectsText} data-testid="jira-config-projects"/>
    </div>
|]
  where
    field :: (JiraConfig -> Text) -> Text
    field getter = maybe "" getter config
    versionIs :: Text -> Bool
    versionIs version = maybe "3" (.apiVersion) config == version
    projectsText :: Text
    projectsText = maybe "" (Text.intercalate ", " . stringList . (.projects)) config

cmdbConfigFormFields :: Maybe CmdbConfig -> Html
cmdbConfigFormFields config =
    [hsx|
    <div class="mb-3">
        <label class="form-label">Name</label>
        <input name="name" type="text" class="form-control" value={field (.name)} data-testid="cmdb-config-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Base URL</label>
        <input name="baseUrl" type="text" class="form-control" value={field (.baseUrl)} data-testid="cmdb-config-base-url" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Token env var</label>
        <input name="tokenEnv" type="text" class="form-control" value={field (.tokenEnv)} data-testid="cmdb-config-token-env" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">Spaces (comma-separated; empty = search all)</label>
        <input name="spaces" type="text" class="form-control" value={spacesText} data-testid="cmdb-config-spaces"/>
    </div>
|]
  where
    field :: (CmdbConfig -> Text) -> Text
    field getter = maybe "" getter config
    spacesText :: Text
    spacesText = maybe "" (Text.intercalate ", " . stringList . (.spaces)) config
