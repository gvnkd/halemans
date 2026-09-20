module Web.View.Audit.Index where

import qualified Data.Aeson as Aeson
import Web.View.Fragments (emptyStateHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView
    { exports :: [AuditExport]
    , users :: [User]
    }

instance View IndexView where
    html IndexView{..} =
        [hsx|
        <div>
        {pageHeaderHtml (tr "Audit exports") mempty}
        <form method="GET" action={ExportAuditAction} class="row g-2 align-items-end mb-4" data-testid="export-form">
            <div class="col-auto">
                <label class="form-label">{tr "From"}</label>
                <input type="text" name="from" class="form-control" placeholder="2026-01-01" data-testid="export-from"/>
            </div>
            <div class="col-auto">
                <label class="form-label">{tr "To"}</label>
                <input type="text" name="to" class="form-control" placeholder="2026-01-08" data-testid="export-to"/>
            </div>
            <div class="col-auto">
                <label class="form-label">{tr "Environment"}</label>
                <input type="text" name="environment" class="form-control" placeholder={tr "all"} data-testid="export-environment"/>
            </div>
            <div class="col-auto">
                <label class="form-label">{tr "Format"}</label>
                <select name="format" class="form-select" data-testid="export-format">
                    <option value="csv">csv</option>
                    <option value="jsonl">jsonl</option>
                </select>
            </div>
            <div class="col-auto">
                <button type="submit" class="btn btn-brand" data-testid="export-submit">{tr "Export"}</button>
            </div>
        </form>
        {tableOrEmpty}
        </div>
    |]
      where
        tableOrEmpty =
            if null exports
                then emptyStateHtml "audit-exports-empty" (tr "No audit exports yet — submit the form above to generate one.")
                else
                    [hsx|
        <table class="table" data-testid="audit-exports-table">
            <thead>
                <tr>
                    <th>{tr "When"}</th>
                    <th>{tr "Who"}</th>
                    <th>{tr "Scope"}</th>
                    <th>{tr "Format"}</th>
                    <th>{tr "Rows"}</th>
                </tr>
            </thead>
            <tbody>
                {forEach exports (renderExportRow users)}
            </tbody>
        </table>
                    |]

renderExportRow :: [User] -> AuditExport -> Html
renderExportRow users export =
    let who = case export.userId >>= (\userId -> find (\user -> get #id user == userId) users) of
            Just user -> user.email
            Nothing -> tr "system"
        scope = cs (Aeson.encode export.scope) :: Text
        rowCount = show export.rowCount :: Text
     in [hsx|
    <tr data-testid="audit-export-row">
        <td>{utcTimeHtml export.createdAt}</td>
        <td>{who}</td>
        <td><code>{scope}</code></td>
        <td>{export.format}</td>
        <td>{rowCount}</td>
    </tr>
|]
