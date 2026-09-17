module Web.View.Sessions.New where

import IHP.AuthSupport.View.Sessions.New
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

instance View (NewView User) where
    html NewView{..} =
        [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div class="mx-auto mb-5 maxw-400">
                        <div class="text-center mb-3">
                            <img src={assetPath "/halemans-app-icon-192.png"} alt="" class="login-glyph"/>
                        </div>
                        <h5>{tr "Halemans — sign in"}</h5>
                        {renderForm user}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: (CurrentUserRecord ~ User, ?request :: Request) => User -> Html
renderForm user =
    [hsx|
    <form method="POST" action={CreateSessionAction} data-testid="login-form">
        <div class="mb-3">
            <input name="email" value={user.email} type="email" class="form-control" placeholder={tr "E-Mail"} required="required" autofocus="autofocus" data-testid="login-email" />
        </div>
        <div class="mb-3">
            <input name="password" type="password" class="form-control" placeholder={tr "Password"} data-testid="login-password" />
        </div>
        <button type="submit" class="btn btn-primary w-100" data-testid="login-submit">{tr "Login"}</button>
    </form>
|]
