module Web.View.Blackouts.Index where

import Web.View.Fragments (editDeleteActionsHtml, emptyStateHtml, pageHeaderHtml)
import Web.View.Prelude

data IndexView = IndexView {blackouts :: [(Blackout, Text)]}

instance View IndexView where
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Blackouts") newButton}
        {tableOrEmpty}
    |]
      where
        tableOrEmpty =
            if null blackouts
                then emptyStateHtml "blackouts-empty" (tr "No blackouts scheduled — new alerts notify as usual.")
                else
                    [hsx|
        <table class="table" data-testid="blackouts-table">
            <thead>
                <tr>
                    <th>{tr "Scope"}</th>
                    <th>{tr "Starts"}</th>
                    <th>{tr "Ends"}</th>
                    <th>{tr "Reason"}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach blackouts renderBlackout}
            </tbody>
        </table>
                    |]
        newButton = [hsx|<a href={NewBlackoutAction} class="btn btn-brand" data-testid="new-blackout">{tr "New blackout"}</a>|]

renderBlackout :: (Blackout, Text) -> Html
renderBlackout (blackout, scopeName) =
    [hsx|
    <tr data-testid="blackout-row">
        <td>{scopeName}</td>
        <td>{utcTimeHtml blackout.startsAt}</td>
        <td>{utcTimeHtml blackout.endsAt}</td>
        <td>{blackout.reason}</td>
        <td>
            {editDeleteActionsHtml (pathTo (EditBlackoutAction blackout.id)) (pathTo (DeleteBlackoutAction blackout.id)) "edit-blackout"}
        </td>
    </tr>
|]
