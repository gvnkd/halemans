module Web.View.Audit.Index where
import Web.View.Prelude
import qualified Data.Aeson as Aeson

data IndexView = IndexView
    { exports :: [AuditExport]
    , users :: [User]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Audit exports</h1>
        <form method="GET" action={ExportAuditAction} class="row g-2 align-items-end mb-4" data-testid="export-form">
            <div class="col-auto">
                <label class="form-label">From</label>
                <input type="text" name="from" class="form-control" placeholder="2026-01-01" data-testid="export-from"/>
            </div>
            <div class="col-auto">
                <label class="form-label">To</label>
                <input type="text" name="to" class="form-control" placeholder="2026-01-08" data-testid="export-to"/>
            </div>
            <div class="col-auto">
                <label class="form-label">Environment</label>
                <input type="text" name="environment" class="form-control" placeholder="all" data-testid="export-environment"/>
            </div>
            <div class="col-auto">
                <label class="form-label">Format</label>
                <select name="format" class="form-select" data-testid="export-format">
                    <option value="csv">csv</option>
                    <option value="jsonl">jsonl</option>
                </select>
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-primary" data-testid="export-submit">Export</button>
            </div>
        </form>
        <table class="table" data-testid="audit-exports-table">
            <thead>
                <tr>
                    <th>When</th>
                    <th>Who</th>
                    <th>Scope</th>
                    <th>Format</th>
                    <th>Rows</th>
                </tr>
            </thead>
            <tbody>
                {forEach exports (renderExportRow users)}
            </tbody>
        </table>
    |]

renderExportRow :: [User] -> AuditExport -> Html
renderExportRow users export =
    let createdAt = show export.createdAt :: Text
        who = case export.userId >>= (\userId -> find (\user -> get #id user == userId) users) of
            Just user -> user.email
            Nothing -> "system"
        scope = cs (Aeson.encode export.scope) :: Text
        rowCount = show export.rowCount :: Text
    in [hsx|
    <tr data-testid="audit-export-row">
        <td>{createdAt}</td>
        <td>{who}</td>
        <td><code>{scope}</code></td>
        <td>{export.format}</td>
        <td>{rowCount}</td>
    </tr>
|]
