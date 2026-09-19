module Web.Controller.Prelude (
    module Web.Types,
    module Application.Helper.Controller,
    module Application.Helper.I18n,
    module IHP.ControllerPrelude,
    module Generated.Types,
    requirePrivilege,
    ensureNotProtected,
)
where

import Application.Helper.Controller
import Application.Helper.I18n
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

-- | Guard for mutating endpoints on provision-managed config: an item whose
-- `protected` flag is set was provisioned from HALEMANS_PROVISION_CONFIG and
-- may only change there — the admin UI renders it read-only and POSTs are
-- rejected with a 403. `label` names the item in the message.
ensureNotProtected :: (?request :: Request, ?respond :: Respond) => Text -> Bool -> IO ()
ensureNotProtected label itemProtected =
    when itemProtected do
        let page =
                let ?context = ?request
                 in defaultLayout
                        [hsx|
                <div class="mt-5" data-testid="protected-item">
                    <h1>403 — Protected item</h1>
                    <p><code>{label}</code> is managed by provisioning (HALEMANS_PROVISION_CONFIG) and cannot be changed here. Edit the provision file and restart instead.</p>
                </div>
            |]
        respondAndExit (responseLBS status403 [("Content-Type", "text/html; charset=utf-8")] (cs (renderMarkupText page)))
