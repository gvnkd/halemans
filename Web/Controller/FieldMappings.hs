module Web.Controller.FieldMappings where

import Application.Pipeline.Grouping (parseAlertField)
import Control.Monad (void)
import Web.Controller.Prelude
import Web.View.FieldMappings.Edit
import Web.View.FieldMappings.Index
import Web.View.FieldMappings.New

-- Facet override chain CRUD (design_docs/milestone_9.md §2/§3). Mappings are
-- global; edits do not retro-update alerts — the recompute button enqueues a
-- bounded FacetBackfillJob instead.
instance Controller FieldMappingsController where
    beforeAction = ensureIsUser

    action FieldMappingsAction = do
        requirePrivilege "manage_rules"
        mappings <-
            query @FieldMapping
                |> orderByAsc #facet
                |> orderByAsc #rank
                |> fetch
        render IndexView{mappings}
    action NewFieldMappingAction = do
        requirePrivilege "manage_rules"
        render NewView
    action CreateFieldMappingAction = do
        requirePrivilege "manage_rules"
        case validateMappingParams of
            Left err -> do
                setErrorMessage err
                redirectTo NewFieldMappingAction
            Right () -> do
                _ <-
                    newRecord @FieldMapping
                        |> set #facet (param @Text "facet")
                        |> set #rank (param @Int "rank")
                        |> set #kind (param @Text "kind")
                        |> set #key (param @Text "key")
                        |> set #enabled enabledParam
                        |> createRecord
                setSuccessMessage (tr "Field mapping created")
                redirectTo FieldMappingsAction
    action EditFieldMappingAction{fieldMappingId} = do
        requirePrivilege "manage_rules"
        mapping <- fetch fieldMappingId
        render EditView{mapping}
    action UpdateFieldMappingAction{fieldMappingId} = do
        requirePrivilege "manage_rules"
        case validateMappingParams of
            Left err -> do
                setErrorMessage err
                redirectTo EditFieldMappingAction{fieldMappingId}
            Right () -> do
                mapping <- fetch fieldMappingId
                _ <-
                    mapping
                        |> set #facet (param @Text "facet")
                        |> set #rank (param @Int "rank")
                        |> set #kind (param @Text "kind")
                        |> set #key (param @Text "key")
                        |> set #enabled enabledParam
                        |> updateRecord
                setSuccessMessage (tr "Field mapping updated")
                redirectTo FieldMappingsAction
    action DeleteFieldMappingAction{fieldMappingId} = do
        requirePrivilege "manage_rules"
        mapping <- fetch fieldMappingId
        deleteRecord mapping
        setSuccessMessage (tr "Field mapping deleted")
        redirectTo FieldMappingsAction
    action RecomputeFacetsAction = do
        requirePrivilege "manage_rules"
        void do
            newRecord @FacetBackfillJob
                |> createRecord
        setSuccessMessage (tr "Facet recompute enqueued (non-closed alerts, chunked)")
        redirectTo FieldMappingsAction

validateMappingParams :: (?request :: Request, ?respond :: Respond) => Either Text ()
validateMappingParams
    | kindParam `notElem` ["field", "label", "attr"] = Left (tr "kind must be field, label or attr")
    | kindParam == "field" && isNothing (parseAlertField (param @Text "key")) =
        Left (tr "field mappings must reference an alert field: env, host, service, check, severity, status")
    | otherwise = Right ()
  where
    kindParam = param @Text "kind" :: Text

enabledParam :: (?request :: Request, ?respond :: Respond) => Bool
enabledParam = paramOrNothing @Text "enabled" == Just "on"
