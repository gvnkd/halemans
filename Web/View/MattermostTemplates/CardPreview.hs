module Web.View.MattermostTemplates.CardPreview (cardPreviewHtml) where

import Application.Service.Mattermost.Render (MattermostCardPreview (..))
import Web.View.Prelude

-- Mock rendering of the Mattermost root card for the admin templates page:
-- header line (may be empty), colored bar, status line, fields grid, and the
-- Ack action placeholder. Close enough to the real client to judge a template.
cardPreviewHtml :: MattermostCardPreview -> Html
cardPreviewHtml preview =
    [hsx|
    <div class="card" data-testid="mm-card-preview">
        <div class="card-body mm-preview" style={barStyle}>
            {headerLine}
            <div class="mm-preview-status">{preview.mcpStatus}</div>
            <table class="table table-sm mm-preview-fields">
                {forEach preview.mcpFields fieldRow}
        </table>
            {propsList}
            {ackButton}
        </div>
    </div>
    <p class="text-muted small">{tr "Preview with a sample alert (firing, critical)."}</p>
    |]
  where
    barStyle = "border-left: 4px solid " <> preview.mcpColor <> ";"
    headerLine =
        if null preview.mcpHeader
            then mempty
            else [hsx|<div class="mm-preview-header">{preview.mcpHeader}</div>|]
    fieldRow (title, value) =
        [hsx|
        <tr>
            <td class="text-muted small">{title}</td>
            <td>{value}</td>
        </tr>
        |]
    ackButton =
        if preview.mcpAckAction
            then [hsx|<button class="btn btn-sm btn-ghost" disabled>{tr "Ack"}</button>|]
            else mempty
    propsList =
        if null preview.mcpProps
            then mempty
            else [hsx|<div class="mm-preview-props small">{forEach preview.mcpProps propLine}</div>|]
    propLine (key, value) =
        [hsx|<div><code>{key}</code>: {value}</div>|]
