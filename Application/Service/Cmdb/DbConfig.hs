module Application.Service.Cmdb.DbConfig (
    cmdbConfigsFromDb,
    currentCmdbConfigs,
    cmdbConfigsForSource,
    lookupForAlert,
    refreshForAlert,
) where

import Application.Helper.Json (stringList)
import qualified Application.Service.Cmdb as Cmdb
import Generated.Types (Alert, CmdbConfig, CmdbConfig' (..), CmdbEntry, CmdbEntry' (..), Source)
import IHP.Fetch (fetch)
import IHP.ModelSupport (ModelContext)
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, orderByAsc, query)
import System.Environment (lookupEnv)

-- DB-only CMDB config resolution (milestone 10; env fallback removed in
-- 2.0) — separate module because the generated CmdbConfig record shares
-- field names with the service one.

-- Enabled cmdb_configs rows with their token resolved (token_env holds the
-- env var NAME, never the secret). Rows whose env var is unset are skipped.
cmdbConfigsFromDb :: (?modelContext :: ModelContext) => IO [Cmdb.CmdbConfig]
cmdbConfigsFromDb = do
    rows <-
        query @CmdbConfig
            |> filterWhere (#enabled, True)
            |> orderByAsc #name
            |> fetch
    catMaybes <$> forM rows \row -> do
        maybeToken <- lookupEnv (cs (get #tokenEnv row))
        pure case maybeToken of
            Nothing -> Nothing
            Just token ->
                let spaces = stringList (get #spaces row)
                 in Just
                        Cmdb.CmdbConfig
                            { Cmdb.baseUrl = get #baseUrl row
                            , Cmdb.token = cs token
                            , Cmdb.space = fromMaybe "" (head spaces)
                            , Cmdb.spaces = spaces
                            }

-- All usable configs: the enabled DB rows.
currentCmdbConfigs :: (?modelContext :: ModelContext) => IO [Cmdb.CmdbConfig]
currentCmdbConfigs = cmdbConfigsFromDb

-- Alert-scoped resolution: the enabled DB rows, with the source's own scope
-- override (cmdbSpaces) replacing every connection's space list when set.
cmdbConfigsForSource :: (?modelContext :: ModelContext) => Source -> IO [Cmdb.CmdbConfig]
cmdbConfigsForSource source = do
    dbConfigs <- cmdbConfigsFromDb
    pure case Cmdb.sourceSpaceOverride source of
        [] -> dbConfigs
        spaces -> map (applyScope spaces) dbConfigs
  where
    applyScope spaces config =
        config
            { Cmdb.spaces = spaces
            , Cmdb.space = fromMaybe "" (head spaces)
            }

-- Cache-first lookup: fresh rows are served directly; missing/stale rows
-- trigger a Confluence search whose outcome (including negatives) is cached.
-- Every configured connection is searched across ALL its spaces (milestone
-- 10); results merge before the best page is picked. Nothing configured is
-- a silent skip, not a failure; only a total search failure (every config
-- errors) yields Left — the stale row is still rendered then.
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
            then pure (Right Nothing)
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
