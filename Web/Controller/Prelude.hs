module Web.Controller.Prelude (
    module Web.Types,
    module Application.Helper.Controller,
    module IHP.ControllerPrelude,
    module Generated.Types,
    requirePrivilege,
)
where

import Application.Helper.Controller
import Generated.Types
import IHP.ControllerPrelude
import IHP.ControllerSupport (respondAndExit)
import IHP.HSX.Markup (renderMarkupText)
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.HTTP.Types (status403)
import Network.Wai (responseLBS)
import Web.Routes
import Web.Types
import Web.View.Layout (defaultLayout)

-- | Guard for mutating endpoints: renders a styled 403 page through the
-- default layout (themed, with navbar) and aborts the action when the
-- current user lacks the privilege.
requirePrivilege :: (CurrentUserRecord ~ User, ?request :: Request, ?respond :: Respond, ?modelContext :: ModelContext) => Text -> IO ()
requirePrivilege privilege = do
    allowed <- currentUserHasPrivilege privilege
    unless allowed do
        let page =
                let ?context = ?request
                 in defaultLayout
                        [hsx|
                <div class="mt-5" data-testid="forbidden">
                    <h1>403 — Forbidden</h1>
                    <p>Your account lacks the <code>{privilege}</code> privilege.</p>
                </div>
            |]
        respondAndExit (responseLBS status403 [("Content-Type", "text/html; charset=utf-8")] (cs (renderMarkupText page)))
