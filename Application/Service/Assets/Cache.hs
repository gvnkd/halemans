module Application.Service.Assets.Cache
( lookupAssetsForAlert
, refreshAssetsForAlert
, linkedAssetsForAlert
, queryTemplate
, negativeTtlSeconds
, missKey
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch (fetch, fetchOneOrNothing)
import Generated.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Text as Text
import Application.Service.Assets
import Application.Service.Assets.Aql
import Application.Service.Assets.Types (AssetObject (..), ObjectListResult (..), Avatar (..), ObjectAttribute (..), ObjectAttributeValue (..), StatusType (..), flattenAttributes)

-- App-side asset cache + alert linkage (design_docs/milestone_8.md §4). The
-- EnrichAlertJob calls lookupAssetsForAlert as a third, independent soft-fail
-- subsystem next to CMDB and Jira: per enabled assets_configs row the alert
-- subject (host, else service) is rendered into the config's AQL template,
-- the top 5 hits are detail-fetched, upserted into assets_objects and linked
-- to the alert. Misses are cached for 30 min so unknown auto-created stub
-- hosts are not re-queried on every new alert.

negativeTtlSeconds :: NominalDiffTime
negativeTtlSeconds = 30 * 60

topN :: Int
topN = 5

-- Cross-alert negative-cache key on the link row (assets_object_id NULL).
missKey :: Id AssetsConfig -> Text -> Text
missKey configId subject = "miss:" <> tshow configId <> ":" <> subject

subjectOf :: Alert -> Maybe Text
subjectOf alert = alert.host <|> alert.service

lookupAssetsForAlert :: (?modelContext :: ModelContext) => Alert -> IO (Either Text ())
lookupAssetsForAlert = resolveAssets False

-- Manual refresh (card button, any view user): bypasses the negative-cache
-- TTL, same shape as the CMDB refresh (milestone_3.md §4).
refreshAssetsForAlert :: (?modelContext :: ModelContext) => Alert -> IO (Either Text ())
refreshAssetsForAlert = resolveAssets True

resolveAssets :: (?modelContext :: ModelContext) => Bool -> Alert -> IO (Either Text ())
resolveAssets force alert = case subjectOf alert of
    Nothing -> pure (Right ())
    Just subject -> do
        configs <- query @AssetsConfig
            |> filterWhere (#enabled, True)
            |> fetch
        results <- forM configs \config -> lookupWithConfig force alert config subject
        pure case [err | Left err <- results] of
            [] -> Right ()
            (err:_) -> Left err

lookupWithConfig :: (?modelContext :: ModelContext) => Bool -> Alert -> AssetsConfig -> Text -> IO (Either Text ())
lookupWithConfig force alert config subject = do
    now <- getCurrentTime
    let cutoff = addUTCTime (negate negativeTtlSeconds) now
    freshMiss <- if force
        then pure Nothing
        else query @AssetAlertLink
            |> filterWhere (#matchedBy, missKey (get #id config) subject)
            |> filterWhereSql (#createdAt, ">= " <> sqlQuote cutoff)
            |> limit 1
            |> fetchOneOrNothing
    if isJust freshMiss
        then pure (Right ())
        else do
            clientResult <- clientFromConfig config
            case clientResult of
                Left err -> pure (Left err)
                Right client -> do
                    let aql = fillHostTemplate (queryTemplate config) subject
                    searchResult <- searchObjects client aql 1 topN
                    case searchResult of
                        Left err -> pure (Left (describeError err))
                        Right page
                            | null page.listEntries -> do
                                _ <- newRecord @AssetAlertLink
                                    |> set #alertId (get #id alert)
                                    |> set #assetsObjectId Nothing
                                    |> set #matchedBy (missKey (get #id config) subject)
                                    |> createRecord
                                pure (Right ())
                            | otherwise -> do
                                linked <- forM (take topN page.listEntries) \entry ->
                                    linkEntry client config alert subject entry.objectId
                                pure case [err | Left err <- linked] of
                                    [] -> Right ()
                                    (err:_) -> Left err

-- The config template is authoritative; when empty fall back to the default
-- schema + Name like (milestone_8.md §2 example).
queryTemplate :: AssetsConfig -> Text
queryTemplate config
    | not (Text.null config.hostQueryTemplate) = config.hostQueryTemplate
    | not (Text.null config.defaultSchemaName) =
        "objectSchema = \"" <> config.defaultSchemaName <> "\" AND Name like \"{host}\""
    | otherwise = "Name like \"{host}\""

linkEntry :: (?modelContext :: ModelContext) => AssetsClient -> AssetsConfig -> Alert -> Text -> Int64 -> IO (Either Text ())
linkEntry client config alert subject objectId = do
    detail <- objectDetail client objectId
    case detail of
        Left err -> pure (Left (describeError err))
        Right object -> do
            cached <- upsertObject config client object
            existing <- query @AssetAlertLink
                |> filterWhere (#alertId, get #id alert)
                |> filterWhere (#assetsObjectId, Just (get #id cached))
                |> limit 1
                |> fetchOneOrNothing
            when (isNothing existing) do
                _ <- newRecord @AssetAlertLink
                    |> set #alertId (get #id alert)
                    |> set #assetsObjectId (Just (get #id cached))
                    |> set #matchedBy subject
                    |> createRecord
                pure ()
            pure (Right ())

upsertObject :: (?modelContext :: ModelContext) => AssetsConfig -> AssetsClient -> AssetObject -> IO AssetsObject
upsertObject config client object = do
    now <- getCurrentTime
    let wireObjectId = object.objectId
    existing <- query @AssetsObject
        |> filterWhere (#configId, get #id config)
        |> filterWhere (#objectId, fromIntegral wireObjectId)
        |> fetchOneOrNothing
    -- Status badge colour comes from the statustype category (§6); the
    -- category is cached next to the flattened attributes so the card never
    -- calls Assets at render time. Failure to fetch it is non-fatal.
    let statusId = listToMaybe
            [ sid | attribute <- object.objectAttributes
                  , value <- attribute.attrValues
                  , Just sid <- [value.valueStatusId] ]
    statusCategory <- case statusId of
        Nothing -> pure Nothing
        Just sid -> do
            result <- statusType client sid
            pure case result of
                Right resolved -> Just resolved.statusTypeCategory
                Left _ -> Nothing
    let flattened = flattenAttributes object.objectAttributes
            ++ [("StatusCategory", tshow category) | Just category <- [statusCategory]]
        attributesJson = Aeson.object [Key.fromText name Aeson..= value | (name, value) <- flattened]
    let applyFields record = record
            |> set #objectKey object.objectKey
            |> set #label_ object.objectLabel
            |> set #objectTypeName object.objectTypeName
            |> set #attributes attributesJson
            |> set #iconUrl (maybe "" (.avatarUrl16) object.objectAvatar)
            |> set #sourceUrl (deepLink client object.objectKey)
            |> set #fetchedAt now
    case existing of
        Just row -> updateRecord (applyFields row)
        Nothing -> createRecord do
            applyFields (newRecord @AssetsObject)
                |> set #configId (get #id config)
                |> set #objectId (fromIntegral wireObjectId)

-- Web deep link: the REST base (.../rest/assets/latest) maps to the Jira
-- insight object page. Stored verbatim in the cache row (§8.4-style: no
-- reconstruction from parts beyond this base swap).
deepLink :: AssetsClient -> Text -> Text
deepLink client objectKey =
    Text.dropWhileEnd (== '/') (fromMaybe client.clientBaseUrl (Text.stripSuffix "/rest/assets/latest" client.clientBaseUrl))
        <> "/secure/insight/assets/" <> objectKey

-- All cached assets linked to an alert, freshest first (card panel + LLM
-- prompt excerpt read this).
linkedAssetsForAlert :: (?modelContext :: ModelContext) => Alert -> IO [(AssetAlertLink, AssetsObject)]
linkedAssetsForAlert alert = do
    links <- query @AssetAlertLink
        |> filterWhere (#alertId, get #id alert)
        |> filterWhereSql (#assetsObjectId, "IS NOT NULL")
        |> orderByAsc #createdAt
        |> fetch
    catMaybes <$> forM links \link -> case link.assetsObjectId of
        Nothing -> pure Nothing
        Just objectId -> do
            object <- fetchOneOrNothing objectId
            pure ((link,) <$> object)

sqlQuote :: UTCTime -> Text
sqlQuote time = "'" <> tshow time <> "'"
