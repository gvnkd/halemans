module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        <h5>Halemans — sign in</h5>
                        {renderForm user}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = [hsx|
    <form method="POST" action={CreateSessionAction} data-testid="login-form">
        <div class="mb-3">
            <input name="email" value={user.email} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" data-testid="login-email" />
        </div>
        <div class="mb-3">
            <input name="password" type="password" class="form-control" placeholder="Password" data-testid="login-password" />
        </div>
        <button type="submit" class="btn btn-primary w-100" data-testid="login-submit">Login</button>
    </form>
|]
