module Web.Controller.Blackouts where

import qualified Data.Text as Text
import Web.Controller.Prelude
import Web.View.Blackouts.Edit
import Web.View.Blackouts.Index
import Web.View.Blackouts.New

instance Controller BlackoutsController where
    beforeAction = ensureIsUser

    action BlackoutsAction = do
        blackouts <-
            query @Blackout
                |> orderByDesc #startsAt
                |> fetch
        scopeNames <- forM blackouts resolveScopeName
        render IndexView{blackouts = zip blackouts scopeNames}
    action NewBlackoutAction = do
        requirePrivilege "manage_blackouts"
        (environments, hosts, services) <- scopeChoices
        render NewView{..}
    action CreateBlackoutAction = do
        requirePrivilege "manage_blackouts"
        let startsAt = param @UTCTime "startsAt"
            endsAt = param @UTCTime "endsAt"
        let reason = param @Text "reason"
        case parseScopeRef (param @Text "scopeId") of
            Just (environmentRef, hostRef, serviceRef) -> do
                _ <-
                    newRecord @Blackout
                        |> set #environmentId environmentRef
                        |> set #hostId hostRef
                        |> set #serviceId serviceRef
                        |> set #startsAt startsAt
                        |> set #endsAt endsAt
                        |> set #reason reason
                        |> set #createdBy (Just currentUserId)
                        |> createRecord
                setSuccessMessage (tr "Blackout created")
            Nothing -> setErrorMessage (tr "invalid scope")
        redirectTo BlackoutsAction
    action EditBlackoutAction{blackoutId} = do
        requirePrivilege "manage_blackouts"
        blackout <- fetch blackoutId
        (environments, hosts, services) <- scopeChoices
        render EditView{..}
    action UpdateBlackoutAction{blackoutId} = do
        requirePrivilege "manage_blackouts"
        blackout <- fetch blackoutId
        let startsAt = param @UTCTime "startsAt"
            endsAt = param @UTCTime "endsAt"
        let reason = param @Text "reason"
        case parseScopeRef (param @Text "scopeId") of
            Just (environmentRef, hostRef, serviceRef) -> do
                _ <-
                    blackout
                        |> set #environmentId environmentRef
                        |> set #hostId hostRef
                        |> set #serviceId serviceRef
                        |> set #startsAt startsAt
                        |> set #endsAt endsAt
                        |> set #reason reason
                        |> updateRecord
                setSuccessMessage (tr "Blackout updated")
            Nothing -> setErrorMessage (tr "invalid scope")
        redirectTo BlackoutsAction
    action DeleteBlackoutAction{blackoutId} = do
        requirePrivilege "manage_blackouts"
        blackout <- fetch blackoutId
        deleteRecord blackout
        setSuccessMessage (tr "Blackout deleted")
        redirectTo BlackoutsAction

scopeChoices :: (?modelContext :: ModelContext) => IO ([Environment], [Host], [Service])
scopeChoices = do
    environments <- query @Environment |> orderByAsc #name |> fetch
    hosts <- query @Host |> orderByAsc #fqdn |> fetch
    services <- query @Service |> orderByAsc #name |> fetch
    pure (environments, hosts, services)

-- | The scopeId form field arrives as "<type>:<uuid>" (see
-- Web.View.Blackouts.New). Nothing = no valid scope selected.
parseScopeRef :: Text -> Maybe (Maybe (Id Environment), Maybe (Id Host), Maybe (Id Service))
parseScopeRef scopeValue =
    let (scopeType, scopeIdText) = Text.break (== ':') scopeValue
        scopeId = Text.drop 1 scopeIdText
     in case scopeType of
            "environment" -> Just (Just (textToId scopeId), Nothing, Nothing)
            "host" -> Just (Nothing, Just (textToId scopeId), Nothing)
            "service" -> Just (Nothing, Nothing, Just (textToId scopeId))
            _ -> Nothing

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
