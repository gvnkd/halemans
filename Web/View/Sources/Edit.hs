module Web.View.Sources.Edit where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude
import Web.View.Sources.Form (SourceFormValues (..), sourceFormFields)

data EditView = EditView
    { source :: Source
    , tokenEnv :: Text
    , writeBack :: Bool
    , jiraWritable :: Bool
    , cmdbSpaces :: Text
    , jiraProjects :: Text
    , initialHistoryDays :: Text
    , hostGroupScope :: Text
    }

instance View EditView where
    html EditView{..} =
        [hsx|
        {pageHeaderHtml (tr "Edit source") mempty}
        <form method="POST" action={UpdateSourceAction source.id} data-testid="source-edit-form" class="maxw-500">
            {sourceFormFields values}
            <button type="submit" class="btn btn-primary" data-testid="source-submit">{tr "Save"}</button>
        </form>
    |]
      where
        values =
            SourceFormValues
                { formName = source.name
                , formType = get #type_ source
                , formBaseUrl = source.baseUrl
                , formEnv = source.env
                , formPollIntervalSeconds = source.pollIntervalSeconds
                , formTokenEnv = tokenEnv
                , formWriteBack = writeBack
                , formJiraWritable = jiraWritable
                , formCmdbSpaces = cmdbSpaces
                , formJiraProjects = jiraProjects
                , formInitialHistoryDays = initialHistoryDays
                , formHostGroupScope = hostGroupScope
                }
