module Web.View.GroupingRules.Preview where

import Web.View.Prelude

data PreviewView = PreviewView
    { rule :: GroupingRule
    , preview :: [(Alert, Text)]
    }

instance View PreviewView where
    html PreviewView{..} =
        [hsx|
        <h1>Preview: {rule.name}</h1>
        <p class="text-secondary">Recent alerts this rule would group, with the rendered group key.</p>
        <table class="table" data-testid="grouping-rule-preview-table">
            <thead>
                <tr>
                    <th>Alert</th>
                    <th>Group key</th>
                </tr>
            </thead>
            <tbody>
                {forEach preview renderRow}
            </tbody>
        </table>
    |]
      where
        renderRow (alert, key) =
            [hsx|
                <tr>
                    <td><a href={ShowAlertAction (get #id alert)}>{alert.title}</a></td>
                    <td><code>{key}</code></td>
                </tr>
            |]
