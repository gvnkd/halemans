module Web.View.Sessions.New where

import Application.Version (appVersion)
import IHP.AuthSupport.View.Sessions.New
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

instance View (NewView User) where
    html NewView{..} =
        [hsx|
        <div class="h-100" id="sessions-new">
            <div class="login-card">
                <img src={assetPath "/halemans-glyph-darkbg.svg"} alt="" class="login-glyph"/>
                <h1 class="login-title">Halemans <span class="login-title-dim">— {tr "sign in"}</span></h1>
                <p class="login-sub">{tr "Home Alert Management System"}</p>
                {renderForm user}
                <div class="login-foot">
                    <span class="app-version-badge" data-testid="app-version">v{appVersion}</span>
                    <span class="login-hint">{tr "single-node · self-hosted"}</span>
                </div>
            </div>
        </div>
    |]

renderForm :: (CurrentUserRecord ~ User, ?request :: Request) => User -> Html
renderForm user =
    [hsx|
    <form method="POST" action={CreateSessionAction} data-testid="login-form">
        <div class="login-field">
            <label for="login-email-input">{tr "Email"}</label>
            <input id="login-email-input" name="email" value={user.email} type="email" class="form-control" placeholder={tr "E-Mail"} required="required" autofocus="autofocus" data-testid="login-email" />
        </div>
        <div class="login-field">
            <label for="login-password-input">{tr "Password"}</label>
            <input id="login-password-input" name="password" type="password" class="form-control" placeholder={tr "Password"} data-testid="login-password" />
        </div>
        <button type="submit" class="btn btn-brand w-100 login-submit" data-testid="login-submit">{tr "Login"}</button>
    </form>
|]
