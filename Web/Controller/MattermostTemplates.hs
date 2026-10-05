module Web.Controller.MattermostTemplates where

import Application.Service.Mattermost.Render (
    MattermostCardPreview (..),
    mattermostTemplateNames,
    previewCard,
 )
import Control.Monad (void)
import qualified Data.Text as Text
import IHP.TypedSql (sqlExecTyped, typedSql)
import Web.Controller.Prelude
import Web.View.MattermostTemplates.Edit
import Web.View.MattermostTemplates.Index (IndexView (..), TemplateCard (..))

-- Admin → Mattermost page: the five mattermost_* llm_prompt_templates rows
-- presented as the parts of one root card, with a live preview. The rows are
-- the SAME versioned/activation-protected rows the LLM admin page edits
-- (this page is a dedicated lens over them; the LLM index filters them out);
-- saving here appends v+1 and activates it transactionally, like the agent
-- tool. An empty body is meaningful (empty mattermost_root = no header).

instance Controller MattermostTemplatesController where
    beforeAction = ensureIsUser

    action MattermostTemplatesAction = do
        requirePrivilege "manage_rules"
        cards <- forM mattermostTemplateNames \name -> do
            versions <-
                query @LlmPromptTemplate
                    |> filterWhere (#name, name)
                    |> orderByDesc #version
                    |> fetch
            let latest = listToMaybe versions
                active = case [t | t <- versions, t.active] of
                    (t : _) -> Just t
                    [] -> Nothing
            pure
                TemplateCard
                    { cardName = name
                    , cardLatestVersion = maybe 0 (.version) latest
                    , cardActiveVersion = fmap (.version) active
                    , cardActiveBody = fmap (.body) active
                    , cardProtected = maybe False (.protected) latest
                    }
        preview <- previewFromActiveRows
        render IndexView{cards, preview}
    action EditMattermostTemplateAction = do
        requirePrivilege "manage_rules"
        let name = param @Text "name"
        if name `notElem` mattermostTemplateNames
            then redirectTo MattermostTemplatesAction
            else do
                active <- activeRowFor name
                forM_ active \row -> ensureNotProtected (name <> " v" <> tshow row.version) row.protected
                let draftBody = fmap (.body) active
                    draftNotes = active >>= (.notes)
                preview <- draftPreviewFor name draftBody
                render EditView{name, latest = active, draftBody, draftNotes, preview}
    action UpdateMattermostTemplateAction = do
        requirePrivilege "manage_rules"
        let name = param @Text "name"
        if name `notElem` mattermostTemplateNames
            then redirectTo MattermostTemplatesAction
            else do
                let body = param @Text "body"
                    notes = paramOrNothing @Text "notes"
                    previewing = isJust (paramOrNothing @Text "preview")
                versions <-
                    query @LlmPromptTemplate
                        |> filterWhere (#name, name)
                        |> orderByDesc #version
                        |> fetch
                let latest = listToMaybe versions
                forM_ latest \row -> ensureNotProtected (name <> " v" <> tshow row.version) row.protected
                if previewing
                    then do
                        preview <- draftPreviewFor name (Just body)
                        render EditView{name, latest, draftBody = Just body, draftNotes = notes, preview}
                    else do
                        let nextVersion = maybe 1 (\row -> row.version + 1) latest
                        void do
                            newRecord @LlmPromptTemplate
                                |> set #name name
                                |> set #version nextVersion
                                |> set #body body
                                |> set #active False
                                |> set #notes notes
                                |> createRecord
                        withTransaction do
                            void do
                                sqlExecTyped
                                    [typedSql|
                                    UPDATE llm_prompt_templates SET active = false, updated_at = NOW()
                                    WHERE name = ${name}
                                |]
                            void do
                                sqlExecTyped
                                    [typedSql|
                                    UPDATE llm_prompt_templates SET active = true, updated_at = NOW()
                                    WHERE name = ${name} AND version = ${nextVersion}
                                |]
                        setSuccessMessage (trp "Saved {name} v{version} (active)" [("name", name), ("version", tshow nextVersion)])
                        redirectToPath (pathTo EditMattermostTemplateAction <> "?name=" <> name)

-- Active row (if any) for a template name.
activeRowFor :: (?modelContext :: ModelContext) => Text -> IO (Maybe LlmPromptTemplate)
activeRowFor name =
    query @LlmPromptTemplate
        |> filterWhere (#name, name)
        |> filterWhere (#active, True)
        |> fetchOneOrNothing

-- Preview built from the ACTIVE bodies of all five parts.
previewFromActiveRows :: (?modelContext :: ModelContext) => IO MattermostCardPreview
previewFromActiveRows = do
    bodies <- forM mattermostTemplateNames \name -> do
        row <- activeRowFor name
        pure (name, fmap (.body) row)
    pure (previewWith bodies)

-- Preview where the edited part uses the DRAFT body and the other four use
-- their active rows (Nothing = built-in default).
draftPreviewFor :: (?modelContext :: ModelContext) => Text -> Maybe Text -> IO MattermostCardPreview
draftPreviewFor editedName draftBody = do
    bodies <- forM mattermostTemplateNames \name ->
        if name == editedName
            then pure (name, draftBody)
            else do
                row <- activeRowFor name
                pure (name, fmap (.body) row)
    pure (previewWith bodies)

previewWith :: [(Text, Maybe Text)] -> MattermostCardPreview
previewWith bodies =
    previewCard
        (pick "mattermost_root")
        (pick "mattermost_status")
        (pick "mattermost_fields")
        (pick "mattermost_color")
  where
    pick n = fromMaybe Nothing (lookup n bodies)
