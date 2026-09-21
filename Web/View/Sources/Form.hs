module Web.View.Sources.Form (
    SourceFormValues (..),
    defaultSourceFormValues,
    sourceFormFields,
) where

import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

-- Shared new/edit fields for sources (milestone 12 §3).
data SourceFormValues = SourceFormValues
    { formName :: Text
    , formType :: Text
    , formBaseUrl :: Text
    , formEnv :: Text
    , formPollIntervalSeconds :: Int
    , formTokenEnv :: Text
    , formWriteBack :: Bool
    , formJiraWritable :: Bool
    , formCmdbSpaces :: Text
    , formJiraProjects :: Text
    , formInitialHistoryDays :: Text
    , formHostGroupScope :: Text
    }

defaultSourceFormValues :: SourceFormValues
defaultSourceFormValues =
    SourceFormValues
        { formName = ""
        , formType = "webhook"
        , formBaseUrl = ""
        , formEnv = "dev"
        , formPollIntervalSeconds = 30
        , formTokenEnv = ""
        , formWriteBack = False
        , formJiraWritable = False
        , formCmdbSpaces = ""
        , formJiraProjects = ""
        , formInitialHistoryDays = ""
        , formHostGroupScope = "all"
        }

sourceFormFields :: (CurrentUserRecord ~ User, ?request :: Request) => SourceFormValues -> Html
sourceFormFields values =
    [hsx|
    <div class="mb-3">
        <label class="form-label">{tr "Name"}</label>
        <input name="name" type="text" class="form-control" value={values.formName} data-testid="source-name" required="required"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Type"}</label>
        <select name="type" class="select" data-testid="source-type">
            {forEach ["webhook", "zabbix", "grafana", "alertmanager"] typeOption}
        </select>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Base URL"}</label>
        <input name="baseUrl" type="text" class="form-control" value={values.formBaseUrl} data-testid="source-base-url" placeholder="http://127.0.0.1:3001"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Environment"}</label>
        <input name="env" type="text" class="form-control" value={values.formEnv} data-testid="source-env"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Poll interval (seconds)"}</label>
        <input name="pollIntervalSeconds" type="number" class="form-control" value={values.formPollIntervalSeconds} data-testid="source-poll-interval"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Token env var"}</label>
        <input name="tokenEnv" type="text" class="form-control" value={values.formTokenEnv} data-testid="source-token-env" placeholder="GRAFANA_TOKEN"/>
    </div>
    <div class="mb-3 form-check">
        <input name="writeBack" type="checkbox" class="form-check-input" checked={values.formWriteBack} data-testid="source-write-back"/>
        <label class="form-check-label">{tr "Write-back (ack/close propagates to the source)"}</label>
    </div>
    <div class="mb-3 form-check">
        <input name="jiraWritable" type="checkbox" class="form-check-input" checked={values.formJiraWritable} data-testid="source-jira-writable"/>
        <label class="form-check-label">{tr "Jira writable (alerts of this source may create Jira tickets)"}</label>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "CMDB spaces (comma-separated; overrides the connection's scope)"}</label>
        <input name="cmdbSpaces" type="text" class="form-control" value={values.formCmdbSpaces} data-testid="source-cmdb-spaces" placeholder="DEV, OPS"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Jira projects (comma-separated; overrides the connection's scope, first entry is the ticket-creation target)"}</label>
        <input name="jiraProjects" type="text" class="form-control" value={values.formJiraProjects} data-testid="source-jira-projects" placeholder="DEV, OPS"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Initial history (days, zabbix first sync; empty = 1)"}</label>
        <input name="initialHistoryDays" type="number" class="form-control" value={values.formInitialHistoryDays} data-testid="source-history-days" placeholder="1"/>
    </div>
    <div class="mb-3">
        <label class="form-label">{tr "Host group scope (zabbix)"}</label>
        <select name="hostGroupScope" class="select" data-testid="source-host-group-scope">
            <option value="all" selected={values.formHostGroupScope /= "teams"}>{tr "all — fetch every alert"}</option>
            <option value="teams" selected={values.formHostGroupScope == "teams"}>{tr "teams — only host groups configured on teams"}</option>
        </select>
    </div>
|]
  where
    typeOption value = [hsx|<option value={value} selected={values.formType == value}>{value}</option>|]
