module Application.Service.Assets.Icons
( iconForObject
, absoluteIconUrl
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetchOneOrNothing)
import Generated.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.Types (Binary (..))
import Control.Exception (try, SomeException)
import Application.Service.Assets

-- Server-side icon/avatar image cache: the card renders <img> against the
-- app (AssetsIconsController), never against the Jira origin, so browsers
-- need no Jira session. Rows in assets_icon_cache are keyed by (config, url)
-- and filled lazily on first request; icons are stable per URL, so cached
-- rows are served indefinitely.

-- Icon/avatar URLs are stored verbatim on the object row (assets-api.md
-- §8.4): relative paths are anchored at the config's Jira host.
absoluteIconUrl :: AssetsConfig -> Text -> Text
absoluteIconUrl config iconUrl
    | Text.null iconUrl = ""
    | "http" `Text.isPrefixOf` iconUrl = iconUrl
    | otherwise = jiraOrigin config <> iconUrl

jiraOrigin :: AssetsConfig -> Text
jiraOrigin config = fromMaybe base (Text.stripSuffix "/rest/assets/latest" base)
    where
        base = Text.dropWhileEnd (== '/') config.baseUrl

-- (content type, body) for the object's icon, from the cache or upstream.
-- Nothing when the object has no icon or the fetch fails (card then renders
-- no image; the failure is logged by the caller's 404).
iconForObject :: (?modelContext :: ModelContext) => AssetsObject -> IO (Maybe (Text, BL.ByteString))
iconForObject object
    | Text.null object.iconUrl = pure Nothing
    | otherwise = do
        config <- fetchOneOrNothing object.configId
        case config of
            Nothing -> pure Nothing
            Just configRecord -> do
                let url = absoluteIconUrl configRecord object.iconUrl
                cached <- cachedIcon configRecord url
                case cached of
                    Just row -> pure (Just (row.contentType, BL.fromStrict (fromBinary row.body)))
                    Nothing -> fetchAndCache configRecord url

cachedIcon :: (?modelContext :: ModelContext) => AssetsConfig -> Text -> IO (Maybe AssetsIconCache)
cachedIcon config url = query @AssetsIconCache
    |> filterWhere (#configId, get #id config)
    |> filterWhere (#url, url)
    |> fetchOneOrNothing

fetchAndCache :: (?modelContext :: ModelContext) => AssetsConfig -> Text -> IO (Maybe (Text, BL.ByteString))
fetchAndCache config url = do
    clientResult <- clientFromConfig config
    case clientResult of
        Left _ -> pure Nothing
        Right client -> do
            fetched <- fetchBinary client url
            case fetched of
                Left _ -> pure Nothing
                Right (contentType, body) -> do
                    -- Concurrent first views race the insert; the unique
                    -- (config_id, url) index keeps one copy, loser re-reads.
                    _ <- try @SomeException do
                        _ <- newRecord @AssetsIconCache
                            |> set #configId (get #id config)
                            |> set #url url
                            |> set #contentType (normalizeContentType contentType)
                            |> set #body (Binary (BL.toStrict body))
                            |> createRecord
                        pure ()
                    pure (Just (normalizeContentType contentType, body))

normalizeContentType :: Text -> Text
normalizeContentType contentType
    | Text.null stripped = "image/png"
    | otherwise = stripped
    where
        stripped = Text.strip (Text.takeWhile (/= ';') contentType)
