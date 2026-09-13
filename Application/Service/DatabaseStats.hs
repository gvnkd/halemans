module Application.Service.DatabaseStats where

import Data.Int (Int64)
import qualified Hasql.Session as Hasql
import IHP.ModelSupport
import IHP.Prelude
import IHP.TypedSql (sqlQueryTyped, typedSql)

data TableStats = TableStats
    { tableName :: Text
    , liveTuples :: Int64
    , deadTuples :: Int64
    , totalBytes :: Int64
    , lastVacuum :: Maybe UTCTime
    , lastAnalyze :: Maybe UTCTime
    }

data DatabaseStats = DatabaseStats
    { databaseName :: Text
    , databaseBytes :: Int64
    , tables :: [TableStats]
    }

fetchDatabaseStats :: (?modelContext :: ModelContext) => IO DatabaseStats
fetchDatabaseStats = do
    dbRows <-
        sqlQueryTyped
            [typedSql|
        SELECT current_database()::text AS db_name,
               pg_database_size(current_database()) AS db_bytes |]
    tableRows <-
        sqlQueryTyped
            [typedSql|
        SELECT relname::text, n_live_tup, n_dead_tup,
               pg_total_relation_size(relid) AS total_bytes,
               greatest(last_vacuum, last_autovacuum) AS last_vacuum,
               greatest(last_analyze, last_autoanalyze) AS last_analyze
        FROM pg_stat_user_tables
        ORDER BY pg_total_relation_size(relid) DESC |]
    let (dbName, dbBytes) = case dbRows of
            (row : _) -> (fromMaybe "?" (get #db_name row), fromMaybe 0 (get #db_bytes row))
            [] -> ("?", 0)
        tables =
            map
                ( \row ->
                    TableStats
                        { tableName = fromMaybe "?" (get #relname row)
                        , liveTuples = fromMaybe 0 (get #n_live_tup row)
                        , deadTuples = fromMaybe 0 (get #n_dead_tup row)
                        , totalBytes = fromMaybe 0 (get #total_bytes row)
                        , lastVacuum = get #last_vacuum row
                        , lastAnalyze = get #last_analyze row
                        }
                )
                tableRows
    pure DatabaseStats{databaseName = dbName, databaseBytes = dbBytes, tables}

tableNames :: (?modelContext :: ModelContext) => IO [Text]
tableNames = do
    rows <- sqlQueryTyped [typedSql| SELECT relname::text FROM pg_stat_user_tables |]
    pure (catMaybes rows)

-- VACUUM/ANALYZE can't go through typedSql: PostgreSQL refuses to PREPARE
-- VACUUM, and the extended query protocol rejects it too. Hasql.script uses
-- the simple query protocol instead, so it's the sanctioned raw-SQL escape.
analyzeDatabase :: (?modelContext :: ModelContext) => IO ()
analyzeDatabase = runSessionHasql ?modelContext.hasqlPool (Hasql.script "ANALYZE")

vacuumAnalyzeDatabase :: (?modelContext :: ModelContext) => IO ()
vacuumAnalyzeDatabase = runSessionHasql ?modelContext.hasqlPool (Hasql.script "VACUUM ANALYZE")

-- Per-table ANALYZE: identifiers can't be bound parameters, so the name is
-- whitelisted against pg_stat_user_tables before interpolation.
analyzeTable :: (?modelContext :: ModelContext) => Text -> IO Bool
analyzeTable tableName = do
    valid <- tableNames
    if tableName `elem` valid
        then do
            runSessionHasql ?modelContext.hasqlPool (Hasql.script ("ANALYZE \"" <> tableName <> "\""))
            pure True
        else pure False
