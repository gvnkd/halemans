module Web.View.FieldMappings.Index where

import Web.View.Fragments (editDeleteActionsHtml, enabledBadgeHtml, inlinePostFormHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {mappings :: [FieldMapping]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml "Field mappings" headerActions}
        <p class="text-secondary">
            Facet override chain: for each facet the first mapping (by ascending rank) that yields a
            non-empty value wins. The key is an unprefixed name read according to the kind:
            <code>field</code> = alert column (env, host, service, check, severity, status),
            <code>label</code> = alerts.labels key, <code>attr</code> = linked Assets object attribute
            (comma-separated list attributes yield their first element).
            Facets are materialized on the alert and drive dashboards and grouping rules.
            A facet named exactly <code>env</code>, <code>host</code> or <code>service</code>
            overrides the raw alert field everywhere: the alert card and list tables show the
            effective value (raw value in parentheses), list/env filters, overview cards, the
            JSON API and metrics all follow the override. <code>check</code>, <code>severity</code>
            and <code>status</code> are never overridable, and blackouts/inventory
            (environments/hosts/services tables) always use the raw ingest values.
            Edits apply to newly ingested/enriched alerts; use recompute to backfill non-closed alerts
            (requires the worker; only alerts with linked Assets objects gain attr facets).
        </p>
        <table class="table" data-testid="field-mappings-table">
            <thead>
                <tr>
                    <th>Facet</th>
                    <th>Rank</th>
                    <th>Kind</th>
                    <th>Key</th>
                    <th>Enabled</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach mappings renderMapping}
            </tbody>
        </table>
    |]
      where
        headerActions =
            [hsx|
                <div>
                    {inlinePostFormHtml (pathTo RecomputeFacetsAction) "Recompute facets" "btn btn-sm btn-outline-warning" (Just "recompute-facets") False}
                    <a href={NewFieldMappingAction} class="btn btn-sm btn-primary" data-testid="new-field-mapping">New mapping</a>
                </div>
            |]

renderMapping :: FieldMapping -> Html
renderMapping mapping =
    [hsx|
    <tr data-testid="field-mapping-row">
        <td data-testid="field-mapping-facet">{mapping.facet}</td>
        <td>{mapping.rank}</td>
        <td><code>{mapping.kind}</code></td>
        <td><code>{mapping.key}</code></td>
        <td>{enabledBadgeHtml mapping.enabled}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditFieldMappingAction mapping.id)) (pathTo (DeleteFieldMappingAction mapping.id)) "edit-field-mapping"}
        </td>
    </tr>
|]
