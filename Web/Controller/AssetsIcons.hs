module Web.Controller.AssetsIcons where

import Web.Controller.Prelude
import Application.Service.Assets.Icons (iconForObject)
import Network.Wai (responseLBS)
import Network.HTTP.Types (status200, status404, hCacheControl, hContentType)

-- Serves cached Jira Assets icon/avatar images from the app (assets-api.md
-- §8.4): browsers never talk to the Jira origin, so no Jira session or CORS
-- is needed. Misses are fetched upstream with the config token and cached in
-- assets_icon_cache (Application.Service.Assets.Icons).
instance Controller AssetsIconsController where
    beforeAction = ensureIsUser

    action ShowAssetIconAction { objectId } = do
        requirePrivilege "view"
        object <- fetchOneOrNothing objectId
        icon <- maybe (pure Nothing) iconForObject object
        case icon of
            Just (contentType, body) ->
                respondWith $ responseLBS status200
                    [ (hContentType, cs contentType)
                    , (hCacheControl, "private, max-age=86400")
                    ] body
            Nothing -> respondWith $ responseLBS status404 [] ""
