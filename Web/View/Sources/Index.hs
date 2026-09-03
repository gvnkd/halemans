module Web.View.Sources.Index where
import Web.View.Prelude

data IndexView = IndexView { sources :: [Source] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Sources</h1>
        <table class="table" data-testid="sources-table">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Type</th>
                    <th>Base URL</th>
                    <th>Env</th>
                    <th>Enabled</th>
                    <th>Poll interval</th>
                </tr>
            </thead>
            <tbody>
                {forEach sources renderSourceRow}
            </tbody>
        </table>
    |]

renderSourceRow :: Source -> Html
renderSourceRow source =
    let sourceType = get #type_ source :: Text
    in [hsx|
    <tr data-source-type={sourceType}>
        <td>{source.name}</td>
        <td>{sourceType}</td>
        <td>{source.baseUrl}</td>
        <td>{source.env}</td>
        <td>{show source.enabled}</td>
        <td>{source.pollIntervalSeconds}s</td>
    </tr>
|]
