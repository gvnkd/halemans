module Web.Controller.Profile where

import Web.Controller.Prelude
import Web.View.Profile.Show
import Application.Service.Push (vapidPublicKey)

instance Controller ProfileController where
    beforeAction = ensureIsUser

    action ProfileAction = do
        subscriptions <- query @PushSubscription
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
        pushPublicKey <- vapidPublicKey
        render ShowView { .. }
