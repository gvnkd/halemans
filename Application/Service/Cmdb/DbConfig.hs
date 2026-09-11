module Application.Service.Cmdb.DbConfig
( cmdbConfigsFromDb
, currentCmdbConfigs
, cmdbConfigsForSource
, lookupForAlert
, refreshForAlert
) where

import IHP.Prelude
import IHP.ModelSupport (ModelContext)
import IHP.QueryBuilder (query, filterWhere, orderByAsc)
import IHP.Fetch (fetch)
import Generated.Types (Source, Alert, CmdbEntry, CmdbEntry' (..), CmdbConfig, CmdbConfig' (..))
import qualified Application.Service.Cmdb as Cmdb
import Data.Aeson (Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as Aeson
import System.Environment (lookupEnv)

-- DB-first CMDB config resolution (milestone 10), same shape as
-- Application.Service.Llm.DbConfig — separate module because the generated
-- CmdbConfig record shares field names with the service one. DB enabled rows
-- win; the env-based per-source fallback keeps pre-milestone-10 installs
-- working unchanged.

-- Enabled cmdb_configs rows with their token resolved (token_env holds the
-- env var NAME, never the secret). Rows whose env var is unset are skipped.
cmdbConfigsFromDb :: (?modelContext :: ModelContext) => IO [Cmdb.CmdbConfig]
cmdbConfigsFromDb = do
    rows <- query @CmdbConfig
        |> filterWhere (#enabled, True)
        |> orderByAsc #name
        |> fetch
    catMaybes <$> forM rows \row -> do
        maybeToken <- lookupEnv (cs (get #tokenEnv row))
        pure case maybeToken of
            Nothing -> Nothing
            Just token ->
                let spaces = stringList (get #spaces row)
                in Just Cmdb.CmdbConfig
                    { Cmdb.baseUrl = get #baseUrl row
                    , Cmdb.token = cs token
                    , Cmdb.space = fromMaybe "" (head spaces)
                    , Cmdb.spaces = spaces
                    }

stringList :: Value -> [Text]
stringList value = fromMaybe [] (parseMaybe Aeson.parseJSON value)

-- All usable configs: enabled DB rows, else the legacy env config taken from
-- the first source that has credentials (pre-milestone-10 behaviour).
currentCmdbConfigs :: (?modelContext :: ModelContext) => IO [Cmdb.CmdbConfig]
currentCmdbConfigs = do
    dbConfigs <- cmdbConfigsFromDb
    if null dbConfigs
        then do
            sources <- query @Source |> fetch
            envConfigs <- forM sources Cmdb.cmdbConfigFromEnv
            pure (maybeToList (foldr (<|>) Nothing envConfigs))
        else pure dbConfigs

-- Alert-scoped resolution: DB rows when present, otherwise the source's own
-- env-based config (per-source cmdbSpace honoured by the fallback).
cmdbConfigsForSource :: (?modelContext :: ModelContext) => Source -> IO [Cmdb.CmdbConfig]
cmdbConfigsForSource source = do
    dbConfigs <- cmdbConfigsFromDb
    if null dbConfigs
        then maybeToList <$> Cmdb.cmdbConfigFromEnv source
        else pure dbConfigs

-- Cache-first lookup: fresh rows are served directly; missing/stale rows
-- trigger a Confluence search whose outcome (including negatives) is cached.
-- Every configured connection is searched across ALL its spaces (milestone
-- 10); results merge before the best page is picked. Only a total failure
-- (every config errors) yields Left — the stale row is still rendered then.
lookupForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
lookupForAlert = resolve False

-- Manual refresh: bypasses TTL, still serves the stale row on failure.
refreshForAlert :: (?modelContext :: ModelContext) => Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
refreshForAlert = resolve True

resolve :: (?modelContext :: ModelContext) => Bool -> Source -> Alert -> IO (Either Text (Maybe CmdbEntry))
resolve force source alert = case Cmdb.subjectOf alert of
    Nothing -> pure (Right Nothing)
    Just subject -> do
        configs <- cmdbConfigsForSource source
        if null configs
            then pure (Left "confluence not configured")
            else do
                now <- getCurrentTime
                cached <- Cmdb.fetchCached subject
                let ttl = case cached of
                        Just entry | isNothing entry.pageId -> Cmdb.negativeTtlSeconds
                        _ -> Cmdb.positiveTtlSeconds
                    fresh = case cached of
                        Just entry -> Cmdb.isFresh now entry.fetchedAt ttl
                        Nothing -> False
                if fresh && not force
                    then pure (Right cached)
                    else do
                        results <- forM configs \config ->
                            Cmdb.confluenceSearch config (Cmdb.cqlForSubject (Cmdb.spaces config) (Cmdb.subjectTerm subject))
                        case [err | Left err <- results] of
                            errs | length errs == length configs -> pure (Left (fromMaybe "confluence search failed" (head errs)))
                            _ -> do
                                let pages = concat [found | Right found <- results]
                                    config = fromMaybe (Cmdb.CmdbConfig "" "" "" []) (head configs)
                                entry <- Cmdb.upsertEntry subject (Cmdb.pickBestPage (Cmdb.subjectTerm subject) pages) config
                                pure (Right (Just entry))
