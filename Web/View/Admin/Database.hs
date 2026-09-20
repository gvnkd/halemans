module Web.View.Admin.Database where

import Application.Service.DatabaseStats (DatabaseStats (..), TableStats (..))
import Web.View.Fragments (inlinePostFormHtml, pageHeaderHtml, sectionHeaderHtml)
import Web.View.Prelude

newtype DatabaseView = DatabaseView
    { stats :: DatabaseStats
    }

instance View DatabaseView where
    html DatabaseView{..} =
        [hsx|
        <div>
        {pageHeaderHtml (tr "Database") mempty}
        <p class="text-muted">
            <span data-testid="db-name">{stats.databaseName}</span>
            —
            <span data-testid="db-size">{formatBytes stats.databaseBytes}</span>
        </p>
        <div class="d-flex gap-2 mb-4">
            <form method="POST" action={AdminDbAnalyzeAction}>
                <button type="submit" class="btn btn-ghost" data-testid="db-analyze-all">{tr "ANALYZE all tables"}</button>
            </form>
            <form method="POST" action={AdminDbVacuumAction} data-confirm={tr "Run VACUUM ANALYZE on the whole database? This can take a while."}>
                <button type="submit" class="btn btn-ghost btn-ghost-critical" data-testid="db-vacuum-all">{tr "VACUUM ANALYZE database"}</button>
            </form>
        </div>
        {sectionHeaderHtml (tr "Tables") mempty}
        <table class="table" data-testid="db-tables-table">
            <thead>
                <tr>
                    <th>{tr "Table"}</th>
                    <th>{tr "Live rows"}</th>
                    <th>{tr "Dead rows"}</th>
                    <th>{tr "Total size"}</th>
                    <th>{tr "Last vacuum"}</th>
                    <th>{tr "Last analyze"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach stats.tables renderTableRow}
            </tbody>
        </table>
        </div>
    |]

renderTableRow :: TableStats -> Html
renderTableRow row =
    let live = show row.liveTuples :: Text
     in [hsx|
    <tr data-testid="db-table-row">
        <td><code>{row.tableName}</code></td>
        <td>{live}</td>
        <td>{deadCell}</td>
        <td>{formatBytes row.totalBytes}</td>
        <td>{utcTimeOrHtml (tr "never") row.lastVacuum}</td>
        <td>{utcTimeOrHtml (tr "never") row.lastAnalyze}</td>
        <td>{inlinePostFormHtml (pathTo (AdminDbAnalyzeTableAction row.tableName)) (tr "Analyze") "btn btn-sm btn-ghost" (Just "db-table-analyze") False}</td>
    </tr>
|]
  where
    dead = show row.deadTuples :: Text
    deadCell
        | row.deadTuples > 0 =
            [hsx|<span class="text-warning" data-testid="db-dead-tuples">{dead}</span>|]
        | otherwise = [hsx|<span>{dead}</span>|]

formatBytes :: Int64 -> Text
formatBytes bytes = go (fromIntegral bytes :: Double) ["B", "KiB", "MiB", "GiB", "TiB"]
  where
    go value [unit] = render value unit
    go value (unit : rest)
        | value >= 1024 = go (value / 1024) rest
        | otherwise = render value unit
    go _ [] = ""
    render value unit =
        let rounded = fromIntegral (round (value * 10) :: Int) / 10 :: Double
         in show rounded <> " " <> unit
