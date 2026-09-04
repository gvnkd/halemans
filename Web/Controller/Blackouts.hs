module Web.Controller.Blackouts where

import Web.Controller.Prelude
import Web.View.Blackouts.Index
import Web.View.Blackouts.New
import qualified Data.Text as Text

instance Controller BlackoutsController where
    beforeAction = ensureIsUser

    action BlackoutsAction = do
        blackouts <- query @Blackout
            |> orderByDesc #startsAt
            |> fetch
        scopeNames <- forM blackouts resolveScopeName
        render IndexView { blackouts = zip blackouts scopeNames }

    action NewBlackoutAction = do
        requirePrivilege "manage_blackouts"
        environments <- query @Environment |> orderByAsc #name |> fetch
        hosts <- query @Host |> orderByAsc #fqdn |> fetch
        services <- query @Service |> orderByAsc #name |> fetch
        render NewView { .. }

    action CreateBlackoutAction = do
        requirePrivilege "manage_blackouts"
        let startsAt = param @UTCTime "startsAt"
            endsAt = param @UTCTime "endsAt"
        let reason = param @Text "reason"
        -- scopeId arrives as "<type>:<uuid>" (see Web.View.Blackouts.New).
        let scopeValue = param @Text "scopeId"
            (scopeType, scopeIdText) = Text.break (== ':') scopeValue
            scopeId = Text.drop 1 scopeIdText
        scopeRef <- case scopeType of
            "environment" -> pure (Just (textToId scopeId), Nothing, Nothing)
            "host" -> pure (Nothing, Just (textToId scopeId), Nothing)
            "service" -> pure (Nothing, Nothing, Just (textToId scopeId))
            _ -> pure (Nothing, Nothing, Nothing)
        case scopeRef of
            (environmentRef, hostRef, serviceRef)
                | isJust environmentRef || isJust hostRef || isJust serviceRef -> do
                    _ <- newRecord @Blackout
                        |> set #environmentId environmentRef
                        |> set #hostId hostRef
                        |> set #serviceId serviceRef
                        |> set #startsAt startsAt
                        |> set #endsAt endsAt
                        |> set #reason reason
                        |> set #createdBy (Just currentUserId)
                        |> createRecord
                    setSuccessMessage "Blackout created"
                | otherwise -> setErrorMessage "invalid scope"
        redirectTo BlackoutsAction

    action DeleteBlackoutAction { blackoutId } = do
        requirePrivilege "manage_blackouts"
        blackout <- fetch blackoutId
        deleteRecord blackout
        setSuccessMessage "Blackout deleted"
        redirectTo BlackoutsAction

resolveScopeName :: (?modelContext :: ModelContext) => Blackout -> IO Text
resolveScopeName blackout = case (blackout.environmentId, blackout.hostId, blackout.serviceId) of
    (Just environmentId, _, _) -> do
        environment <- fetch environmentId
        pure ("env: " <> environment.name)
    (_, Just hostId, _) -> do
        host <- fetch hostId
        pure ("host: " <> host.fqdn)
    (_, _, Just serviceId) -> do
        service <- fetch serviceId
        pure ("service: " <> service.name)
    _ -> pure "-"
