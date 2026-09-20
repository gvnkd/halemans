module Web.View.GroupingRules.Preview where

import Web.View.Fragments (pageHeaderHtml)
import Web.View.Prelude

data PreviewView = PreviewView
    { rule :: GroupingRule
    , preview :: [(Alert, Text)]
    }

instance View PreviewView where
    html PreviewView{..} =
        [hsx|
        {pageHeaderHtml (tr "Preview" <> ": " <> rule.name) mempty}
        <p class="text-secondary">{tr "Recent alerts this rule would group, with the rendered group key."}</p>
        <table class="table" data-testid="grouping-rule-preview-table">
            <thead>
                <tr>
                    <th>{tr "Alert"}</th>
                    <th>{tr "Group key"}</th>
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
