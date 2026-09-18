module Web.View.AssetsAdmin.Form (assetsConfigFormFields) where

import Web.View.Prelude

-- Shared new/edit fields for assets_configs (milestone_8.md §8).
assetsConfigFormFields :: Maybe AssetsConfig -> Html
assetsConfigFormFields config =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Name"}</label>
        <input name="name" type="text" class="form-control" value={field (.name)} data-testid="assets-config-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Base URL (.../rest/assets/latest)"}</label>
        <input name="baseUrl" type="text" class="form-control" value={field (.baseUrl)} data-testid="assets-config-base-url" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Token env var"}</label>
        <input name="tokenEnv" type="text" class="form-control" value={field (.tokenEnv)} data-testid="assets-config-token-env" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Auth mode"}</label>
        <select name="authMode" class="form-select" data-testid="assets-config-auth-mode">
            <option value="bearer" selected={modeIs "bearer"}>bearer</option>
            <option value="basic" selected={modeIs "basic"}>basic</option>
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Jira email env var (basic auth)"}</label>
        <input name="jiraEmailEnv" type="text" class="form-control" value={fromMaybe "" (config >>= (.jiraEmailEnv))} data-testid="assets-config-jira-email-env"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Default schema name (AQL objectSchema = \"...\")"}</label>
        <input name="defaultSchemaName" type="text" class="form-control" value={field (.defaultSchemaName)} data-testid="assets-config-schema"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Host query template (AQL with {host} placeholder)"}</label>
        <textarea name="hostQueryTemplate" class="form-control" rows="2" data-testid="assets-config-template">{field (.hostQueryTemplate)}</textarea>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Attribute display list (comma-separated)"}</label>
        <input name="attributeNames" type="text" class="form-control" value={attrNames} data-testid="assets-config-attributes"/>
    </div>
|]
  where
    field :: (AssetsConfig -> Text) -> Text
    field getter = maybe "" getter config
    attrNames :: Text
    attrNames = maybe "Owner,Cluster,Database,IP,Datacenter" (.attributeNames) config
    modeIs :: Text -> Bool
    modeIs mode = maybe "bearer" (.authMode) config == mode
