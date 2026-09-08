module Web.View.FieldMappings.Index where
import Web.View.Prelude

data IndexView = IndexView { mappings :: [FieldMapping] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center">
            <h1>Field mappings</h1>
            <div>
                <form method="POST" action={RecomputeFacetsAction} class="d-inline">
                    <button type="submit" class="btn btn-sm btn-outline-warning" data-testid="recompute-facets">Recompute facets</button>
                </form>
                <a href={NewFieldMappingAction} class="btn btn-sm btn-primary" data-testid="new-field-mapping">New mapping</a>
            </div>
        </div>
        <p class="text-secondary">
            Facet override chain: for each facet the first mapping (by ascending rank) that yields a
            non-empty value wins. Kinds: <code>field:</code> alert column, <code>label:</code> alerts.labels key,
            <code>attr:</code> linked Assets object attribute. Edits apply to newly ingested/enriched alerts;
            use recompute to backfill non-closed alerts.
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

renderMapping :: FieldMapping -> Html
renderMapping mapping = [hsx|
    <tr data-testid="field-mapping-row">
        <td data-testid="field-mapping-facet">{mapping.facet}</td>
        <td>{mapping.rank}</td>
        <td><code>{mapping.kind}</code></td>
        <td><code>{mapping.key}</code></td>
        <td>{enabledBadge}</td>
        <td>
            <a href={EditFieldMappingAction mapping.id} class="btn btn-sm btn-outline-secondary" data-testid="edit-field-mapping">Edit</a>
            <form method="POST" action={DeleteFieldMappingAction mapping.id} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-danger">Delete</button>
            </form>
        </td>
    </tr>
|]
    where
        enabledBadge = if mapping.enabled
            then [hsx|<span class="badge bg-success">enabled</span>|]
            else [hsx|<span class="badge bg-secondary">disabled</span>|]
