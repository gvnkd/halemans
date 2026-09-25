module Web.Controller.Blackouts where

import qualified Data.Text as Text
import Data.Traversable (traverse)
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
        case scopeParams of
            Just applyScope -> do
                _ <-
                    newRecord @Blackout
                        |> applyScope
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
        case scopeParams of
            Just applyScope -> do
                _ <-
                    blackout
                        |> applyScope
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

-- | Scope portion of the create/update form: either a single inventory ref
-- (scopeId, legacy picker) or shell-style globs (scopeType=pattern). Returns
-- Nothing for an invalid/empty scope. Sets ALL six scope columns so editing
-- between the two kinds can't leave stale legs behind.
scopeParams :: (?request :: Request) => Maybe (Blackout -> Blackout)
scopeParams
    | param @Text "scopeType" == "pattern" =
        if all isNothing [envGlob, hostGlob, serviceGlob]
            then Nothing
            else
                Just
                    ( \blackout ->
                        blackout
                            |> set #environmentId Nothing
                            |> set #hostId Nothing
                            |> set #serviceId Nothing
                            |> set #environmentGlob envGlob
                            |> set #hostGlob hostGlob
                            |> set #serviceGlob serviceGlob
                    )
    | otherwise = do
        (environmentRef, hostRef, serviceRef) <- parseScopeRef (param @Text "scopeId")
        Just
            ( \blackout ->
                blackout
                    |> set #environmentId environmentRef
                    |> set #hostId hostRef
                    |> set #serviceId serviceRef
                    |> set #environmentGlob Nothing
                    |> set #hostGlob Nothing
                    |> set #serviceGlob Nothing
            )
  where
    envGlob = blankToNothing (paramOrNothing @Text "envGlob")
    hostGlob = blankToNothing (paramOrNothing @Text "hostGlob")
    serviceGlob = blankToNothing (paramOrNothing @Text "serviceGlob")
    blankToNothing = maybe Nothing (\value -> if Text.null (Text.strip value) then Nothing else Just value)

resolveScopeName :: (?modelContext :: ModelContext) => Blackout -> IO Text
resolveScopeName blackout = do
    environmentPart <- traverse (fmap (("env: " <>) . (.name)) . fetch) blackout.environmentId
    hostPart <- traverse (fmap (("host: " <>) . (.fqdn)) . fetch) blackout.hostId
    servicePart <- traverse (fmap (("service: " <>) . (.name)) . fetch) blackout.serviceId
    let globParts =
            [ label <> ": " <> glob
            | (label, Just glob) <-
                [ ("env glob", blackout.environmentGlob)
                , ("host glob", blackout.hostGlob)
                , ("service glob", blackout.serviceGlob)
                ]
            ]
    pure case catMaybes [environmentPart, hostPart, servicePart] <> globParts of
        [] -> "-"
        parts -> Text.intercalate " · " parts
