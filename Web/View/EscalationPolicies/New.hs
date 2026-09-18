module Web.View.EscalationPolicies.New where

import Application.Pipeline.Escalation (EscalationStep (..))
import IHP.LoginSupport.Helper.Controller (CurrentUserRecord)
import Network.Wai (Request)
import Web.View.Prelude

data NewView = NewView
    { teams :: [Team]
    , users :: [User]
    }

instance View NewView where
    html NewView{..} =
        [hsx|
        <h1>{tr "New escalation policy"}</h1>
        <form method="POST" action={CreateEscalationPolicyAction} data-testid="escalation-policy-form" class="maxw-700">
            <div class="mb-3">
                <label class="form-label">{tr "Name"}</label>
                <input name="name" type="text" class="form-control" data-testid="policy-name" required="required"/>
            </div>
            {stepEditor teams users []}
            <button type="submit" class="btn btn-primary" data-testid="policy-submit">{tr "Create"}</button>
        </form>
    |]

-- Step list editor: fixed slots, empty rows are dropped server-side. Shared
-- with Edit (prefilled from the stored steps).
stepEditor :: (CurrentUserRecord ~ User, ?request :: Request) => [Team] -> [User] -> [EscalationStep] -> Html
stepEditor teams users steps =
    [hsx|
    <div class="mb-3" data-testid="policy-steps">
        <label class="form-label">{tr "Steps (after → target, optional unless-status)"}</label>
        {forEach (zip [0 .. 4] (padSteps steps)) stepRow}
    </div>
|]
  where
    padSteps existing = take 5 (map Just existing ++ repeat Nothing)
    stepRow (index, maybeStep) =
        let afterValue = maybe "" (show . esAfterSeconds) maybeStep
            targetValue = case maybeStep of
                Just step
                    | Just teamId <- step.esTargetTeamId -> "team:" <> teamId
                    | Just userId <- step.esTargetUserId -> "user:" <> userId
                _ -> ""
            unlessValue = maybe "" (fromMaybe "" . esUnlessStatus) maybeStep
         in [hsx|
                <div class="input-group input-group-sm mb-1" data-testid={"policy-step-" <> show index}>
                    <span class="input-group-text">{tr "after (s)"}</span>
                    <input name="stepAfter" type="number" class="form-control" value={afterValue} data-testid="step-after"/>
                    <select name="stepTarget" class="form-select" data-testid="step-target">
                        <option value="" selected={targetValue == ""}>—</option>
                        {forEach teams (teamOption targetValue)}
                        {forEach users (userOption targetValue)}
                    </select>
                    <select name="stepUnless" class="form-select" data-testid="step-unless">
                        <option value="" selected={unlessValue == ""}>{tr "unless: —"}</option>
                        <option value="ack" selected={unlessValue == "ack"}>{tr "unless ack"}</option>
                    </select>
                </div>
            |]
    teamOption selected team =
        [hsx|
            <option value={"team:" <> tshow (get #id team)} selected={selected == "team:" <> tshow (get #id team)}>{tr "team"}: {team.name}</option>
        |]
    userOption selected user =
        [hsx|
            <option value={"user:" <> tshow (get #id user)} selected={selected == "user:" <> tshow (get #id user)}>{tr "user"}: {user.email}</option>
        |]
