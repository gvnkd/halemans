module Application.Service.DynTable (
    FilterKind (..),
    ColumnFilter (..),
    TableColumn (..),
    TableConfig (..),
    TableState (..),
    parseTableState,
    visibleColumns,
    hiddenFilterColumns,
    filterValues,
    nextSortDirFor,
    tableStateQueryItems,
    pageCountFor,
    pageWindow,
) where

import qualified Data.List as List
import IHP.Prelude
import Text.Read (readMaybe)

-- Generic dynamic-table machinery: a column config (labels, sortability,
-- per-column filters) plus the validated sort/filter/page state parsed from
-- the query string, and serialization back to query items for sort header
-- and pager links. The SQL itself stays per-page (typedSql with the
-- CASE-over-whitelisted-param ORDER BY pattern); everything that is the
-- same for every dynamic table lives here and in Web.View.DynTable.

data FilterKind = FilterText | FilterMulti
    deriving (Eq, Show)

data ColumnFilter = ColumnFilter
    { cfParam :: Text
    , cfKind :: FilterKind
    , cfPlaceholder :: Text
    , cfOptions :: [Text]
    -- ^ FilterMulti checkbox options / FilterText datalist suggestions;
    -- request-scoped (e.g. distinct values of the current page).
    }

data TableColumn = TableColumn
    { colKey :: Text
    , colLabel :: Text
    , colSortable :: Bool
    , colNaturalDir :: Text
    -- ^ Direction a fresh header click sorts by ("asc" | "desc").
    , colFilter :: Maybe ColumnFilter
    }

data TableConfig = TableConfig
    { cfgName :: Text
    -- ^ testid prefix and prefs namespace ("alerts").
    , cfgColumns :: [TableColumn]
    , cfgDefaultVisible :: [Text]
    , cfgDefaultSort :: Text
    , cfgDefaultDir :: Text
    , cfgPageSizes :: [Int]
    , cfgDefaultPageSize :: Int
    , cfgColumnPicker :: Bool
    -- ^ Embedded tables (dashboard cards, group members) turn both off:
    -- their rows are capped/scoped by the surrounding page already.
    , cfgPager :: Bool
    }

data TableState = TableState
    { tsSort :: Text
    , tsDir :: Text
    , tsPage :: Int
    -- ^ 1-based.
    , tsPageSize :: Int
    , tsVisible :: [Text]
    , tsFilters :: [(Text, [Text])]
    -- ^ Filter param name -> selected values (FilterText: singleton).
    }
    deriving (Eq, Show)

-- | Everything user-supplied is validated against the config: unknown sort
-- columns, page sizes and column keys fall back to the defaults.
parseTableState :: TableConfig -> [(ByteString, Maybe ByteString)] -> TableState
parseTableState cfg query =
    TableState
        { tsSort = if requestedSort `elem` sortableKeys then requestedSort else cfg.cfgDefaultSort
        , tsDir = if requestedDir `elem` ["asc", "desc"] then requestedDir else cfg.cfgDefaultDir
        , tsPage = maybe 1 (max 1) requestedPage
        , tsPageSize = if requestedPageSize `elem` cfg.cfgPageSizes then requestedPageSize else cfg.cfgDefaultPageSize
        , tsVisible = visible
        , tsFilters = filters
        }
  where
    valuesFor key = [cs value | (k, Just value) <- query, cs k == key, value /= ""]
    requestedSort = fromMaybe cfg.cfgDefaultSort (listToMaybe (valuesFor "sort"))
    requestedDir = fromMaybe cfg.cfgDefaultDir (listToMaybe (valuesFor "dir"))
    requestedPage = listToMaybe (valuesFor "page") >>= readMaybe . cs
    requestedPageSize = fromMaybe cfg.cfgDefaultPageSize (listToMaybe (valuesFor "pageSize") >>= readMaybe . cs)
    sortableKeys = [col.colKey | col <- cfg.cfgColumns, col.colSortable]
    requestedCols = valuesFor "cols"
    visible
        | null requestedCols = cfg.cfgDefaultVisible
        | otherwise = case [col.colKey | col <- cfg.cfgColumns, col.colKey `elem` requestedCols] of
            [] -> cfg.cfgDefaultVisible
            valid -> valid
    filters =
        [ (cf.cfParam, values)
        | col <- cfg.cfgColumns
        , Just cf <- [col.colFilter]
        , let values = valuesFor cf.cfParam
        , not (null values)
        ]

visibleColumns :: TableConfig -> TableState -> [TableColumn]
visibleColumns cfg state = [col | col <- cfg.cfgColumns, col.colKey `elem` state.tsVisible]

-- | Filterable columns that are currently hidden; the widget renders their
-- inputs in the toolbar so hiding a column never strands its filter.
hiddenFilterColumns :: TableConfig -> TableState -> [TableColumn]
hiddenFilterColumns cfg state =
    [col | col <- cfg.cfgColumns, col.colKey `notElem` state.tsVisible, isJust col.colFilter]

filterValues :: TableState -> Text -> [Text]
filterValues state param = fromMaybe [] (lookup param state.tsFilters)

-- | Direction for a header click: toggles on the active column, otherwise
-- the column's natural direction.
nextSortDirFor :: TableConfig -> TableState -> Text -> Text
nextSortDirFor cfg state column
    | state.tsSort == column = if state.tsDir == "asc" then "desc" else "asc"
    | otherwise = maybe "asc" colNaturalDir (find (\col -> col.colKey == column) cfg.cfgColumns)

-- | Canonical query string for the state: sort/dir always, cols/pageSize
-- only when they deviate from the defaults, page only beyond the first.
tableStateQueryItems :: TableConfig -> TableState -> [(ByteString, Maybe ByteString)]
tableStateQueryItems cfg state =
    concatMap filterItems state.tsFilters
        ++ [("sort", Just (cs state.tsSort)), ("dir", Just (cs state.tsDir))]
        ++ colsItems
        ++ pageSizeItems
        ++ pageItems
  where
    filterItems (param, values) = [(cs param, Just (cs value)) | value <- values]
    colsItems = [("cols", Just (cs col)) | state.tsVisible /= cfg.cfgDefaultVisible, col <- state.tsVisible]
    pageSizeItems = [("pageSize", Just (cs (tshow state.tsPageSize))) | state.tsPageSize /= cfg.cfgDefaultPageSize]
    pageItems = [("page", Just (cs (tshow state.tsPage))) | state.tsPage > 1]

pageCountFor :: Int64 -> Int -> Int
pageCountFor total pageSize = max 1 (fromIntegral ((total + fromIntegral pageSize - 1) `div` fromIntegral pageSize))

-- | Pager pages with Nothing as an ellipsis gap: first/last plus a
-- one-page window around the current page.
pageWindow :: Int -> Int -> [Maybe Int]
pageWindow current totalPages = go 1 pages
  where
    pages = List.nub (List.sort [p | p <- [1, totalPages, current - 1, current, current + 1], p >= 1, p <= totalPages])
    go _ [] = []
    go next (p : rest)
        | p > next = Nothing : Just p : go (p + 1) rest
        | otherwise = Just p : go (p + 1) rest
