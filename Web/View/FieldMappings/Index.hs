module Web.View.FieldMappings.Index where

import Web.View.Fragments (calloutInfoHtml, editDeleteActionsHtml, emptyStateHtml, enabledBadgeHtml, inlinePostFormHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {mappings :: [FieldMapping]}

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Field mappings")
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Field mappings") headerActions}
        {helpCallout}
        {tableOrEmpty}
    |]
      where
        helpCallout =
            calloutInfoHtml
                "field-mappings-help"
                [hsx|
                {tr "Facet override chain: for each facet the first mapping (by ascending rank) that yields a non-empty value wins. The key is an unprefixed name read according to the kind:"}
                <code>field</code> = {tr "alert column (env, host, service, check, severity, status),"}
                <code>label</code> = {tr "alerts.labels key,"} <code>attr</code> = {tr "linked Assets object attribute (comma-separated list attributes yield their first element)."}
                {tr "Facets are materialized on the alert and drive dashboards and grouping rules."}
                {tr "A facet named exactly"} <code>env</code>, <code>host</code> {tr "or"} <code>service</code>
                {tr "overrides the raw alert field everywhere: the alert card and list tables show the effective value (raw value in parentheses), list/env filters, overview cards, the JSON API and metrics all follow the override."}
                <code>check</code>, <code>severity</code> {tr "and"} <code>status</code>
                {tr "are never overridable, and blackouts/inventory (environments/hosts/services tables) always use the raw ingest values."}
                {tr "Edits apply to newly ingested/enriched alerts; use recompute to backfill non-closed alerts (requires the worker; only alerts with linked Assets objects gain attr facets)."}
                |]
        tableOrEmpty =
            if null mappings
                then emptyStateHtml "field-mappings-empty" (tr "No field mappings yet — alerts use their raw field values.")
                else
                    [hsx|
        <table class="table" data-testid="field-mappings-table">
            <thead>
                <tr>
                    <th>{tr "Facet"}</th>
                    <th>{tr "Rank"}</th>
                    <th>{tr "Kind"}</th>
                    <th>{tr "Key"}</th>
                    <th>{tr "Enabled"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach mappings renderMapping}
            </tbody>
        </table>
                    |]
        headerActions =
            [hsx|
                <div>
                    {inlinePostFormHtml (pathTo RecomputeFacetsAction) (tr "Recompute facets") "btn btn-sm btn-ghost" (Just "recompute-facets") False}
                    <a href={NewFieldMappingAction} class="btn btn-brand" data-testid="new-field-mapping">{tr "New mapping"}</a>
                </div>
            |]

renderMapping :: FieldMapping -> Html
renderMapping mapping =
    [hsx|
    <tr data-testid="field-mapping-row">
        <td data-testid="field-mapping-facet">{mapping.facet} {protectedBadgeHtml (get #protected mapping)}</td>
        <td>{mapping.rank}</td>
        <td><code>{mapping.kind}</code></td>
        <td><code>{mapping.key}</code></td>
        <td>{enabledBadgeHtml mapping.enabled}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditFieldMappingAction mapping.id)) (pathTo (DeleteFieldMappingAction mapping.id)) "edit-field-mapping"}
        </td>
    </tr>
|]
