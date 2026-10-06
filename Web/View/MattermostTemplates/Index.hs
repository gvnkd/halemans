module Web.View.MattermostTemplates.Index where

import Application.Service.Mattermost.Render (MattermostCardPreview)
import Web.View.Fragments (pageHeaderHtml)
import Web.View.MattermostTemplates.CardPreview (cardPreviewHtml)
import Web.View.Prelude

data TemplateCard = TemplateCard
    { cardName :: Text
    , cardLatestVersion :: Int
    , cardActiveVersion :: Maybe Int
    , cardActiveBody :: Maybe Text
    , cardProtected :: Bool
    }

data IndexView = IndexView
    { cards :: [TemplateCard]
    , preview :: MattermostCardPreview
    }

instance View IndexView where
    beforeRender _ = setPageTitle (tr "Mattermost card")
    html IndexView{..} =
        [hsx|
        {pageHeaderHtml (tr "Mattermost card templates") mempty}
        <p class="text-muted">{tr "The alert card posted to Mattermost is assembled from these five parts. Saving a part appends a new version and activates it; a broken part falls back to the built-in default and never drops the notification."}</p>
        <h6 class="mt-4">{tr "Live preview (active parts)"}</h6>
        {cardPreviewHtml preview}
        <div class="row mt-4">
            {forEach cards cardHtml}
        </div>
        |]

cardHtml :: TemplateCard -> Html
cardHtml card =
    [hsx|
    <div class="col-md-6 col-lg-4 mb-3">
        <div class="card h-100" data-testid={"mm-tpl-card-" <> card.cardName}>
            <div class="card-body">
                <h6 class="card-heading">{partLabel card.cardName}</h6>
                <p class="text-muted small mb-1"><code>{card.cardName}</code></p>
                <p class="small mb-2">{versionLine}</p>
                <pre class="mm-tpl-body small">{bodyText}</pre>
                <a href={editUrl} class="btn btn-sm btn-brand" data-testid={"mm-tpl-edit-" <> card.cardName}>{tr "Edit"}</a>
            </div>
        </div>
    </div>
    |]
  where
    versionLine = case card.cardActiveVersion of
        Just v ->
            trp "active v{version} (latest v{latest})" [("version", tshow v), ("latest", tshow card.cardLatestVersion)]
                <> (if card.cardProtected then " · " <> tr "provisioned" else "")
        Nothing -> tr "no active version — the built-in default is used"
    bodyText = fromMaybe "" card.cardActiveBody
    editUrl = pathTo EditMattermostTemplateAction <> "?name=" <> card.cardName

partLabel :: (?request :: Request) => Text -> Text
partLabel name = case name of
    "mattermost_root" -> tr "Header line"
    "mattermost_details" -> tr "Details reply (thread)"
    "mattermost_status" -> tr "Status line"
    "mattermost_fields" -> tr "Fields grid"
    "mattermost_color" -> tr "Color bar"
    "mattermost_attachment" -> tr "Extra attachment properties"
    _ -> name
