module Web.View.Environments.Show where

import Application.Helper.DashboardConfig (alertListPageSizes, defaultAlertListColumns, defaultAlertListPageSize)
import Application.Service.AlertList (parseRelativeWindow, validColumns, validSortColumns)
import Application.Service.DynTable
import Data.Aeson ((.!=), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Types.URI (renderQuery)
import Web.View.DynTable (DynTable (..), dynTableHtml)
import Web.View.Fragments (alertListColumns, alertRowHtmlCols, groupedAlertsTableHtml)
import Web.View.Prelude

data EnvFilters = EnvFilters
    { filterSeverities :: [Text]
    , filterStatuses :: [Text]
    , filterHost :: Maybe Text
    , filterService :: Maybe Text
    , filterText :: Maybe Text
    , filterGroup :: Maybe Text
    , filterSort :: Text
    , filterDir :: Text
    , filterCols :: [Text]
    , filterPage :: Int
    , filterPageSize :: Int
    , filterOccMin :: Maybe Int
    , filterSeenWithin :: Maybe Text
    }
    deriving (Eq, Show)

emptyEnvFilters :: EnvFilters
emptyEnvFilters =
    EnvFilters
        { filterSeverities = []
        , filterStatuses = []
        , filterHost = Nothing
        , filterService = Nothing
        , filterText = Nothing
        , filterGroup = Nothing
        , filterSort = "last_seen_at"
        , filterDir = "desc"
        , filterCols = defaultAlertListColumns
        , filterPage = 1
        , filterPageSize = defaultAlertListPageSize
        , filterOccMin = Nothing
        , filterSeenWithin = Nothing
        }

-- Persisted shape for users.settings.filters.env (view mode included; the
-- page itself is deliberately not persisted).
envFiltersToValue :: EnvFilters -> Text -> Aeson.Value
envFiltersToValue filters viewMode =
    Aeson.object
        [ "severity" .= filters.filterSeverities
        , "status" .= filters.filterStatuses
        , "host" .= filters.filterHost
        , "service" .= filters.filterService
        , "q" .= filters.filterText
        , "group" .= filters.filterGroup
        , "sort" .= filters.filterSort
        , "dir" .= filters.filterDir
        , "cols" .= filters.filterCols
        , "pageSize" .= filters.filterPageSize
        , "occ_min" .= filters.filterOccMin
        , "seen" .= filters.filterSeenWithin
        , "view" .= viewMode
        ]

envFiltersFromValue :: Aeson.Value -> Maybe (EnvFilters, Text)
envFiltersFromValue =
    parseMaybe
        ( Aeson.withObject "envFilters" \o -> do
            severities <- o Aeson..:? "severity" .!= []
            statuses <- o Aeson..:? "status" .!= []
            host <- nonEmptyField o "host"
            service <- nonEmptyField o "service"
            title <- nonEmptyField o "q"
            group <- nonEmptyField o "group"
            sort :: Text <- o Aeson..:? "sort" .!= "last_seen_at"
            dir :: Text <- o Aeson..:? "dir" .!= "desc"
            cols <- o Aeson..:? "cols" .!= []
            pageSize <- o Aeson..:? "pageSize" .!= defaultAlertListPageSize
            occMin <- o Aeson..:? "occ_min"
            seenRaw <- o Aeson..:? "seen"
            let seen = seenRaw >>= \w -> if isJust (parseRelativeWindow w) then Just w else Nothing
            view :: Text <- o Aeson..:? "view" .!= "flat"
            pure
                ( emptyEnvFilters
                    { filterSeverities = severities
                    , filterStatuses = statuses
                    , filterHost = host
                    , filterService = service
                    , filterText = title
                    , filterGroup = group
                    , filterSort = if sort `elem` validSortColumns then sort else "last_seen_at"
                    , filterDir = if dir == "asc" then "asc" else "desc"
                    , filterCols = validColumns cols
                    , filterPageSize = if pageSize `elem` alertListPageSizes then pageSize else defaultAlertListPageSize
                    , filterOccMin = occMin
                    , filterSeenWithin = seen
                    }
                , if view == "grouped" then "grouped" else "flat"
                )
        )
  where
    nonEmptyField o key = do
        raw <- o Aeson..:? key .!= ""
        pure (if raw == "" then Nothing else Just raw)

envBaseItems :: EnvFilters -> Text -> [(ByteString, Maybe ByteString)]
envBaseItems f viewMode =
    map (\value -> ("severity", Just (cs value))) f.filterSeverities
        ++ map (\value -> ("status", Just (cs value))) f.filterStatuses
        ++ maybe [] (\value -> [("host", Just (cs value))]) f.filterHost
        ++ maybe [] (\value -> [("service", Just (cs value))]) f.filterService
        ++ maybe [] (\value -> [("q", Just (cs value))]) f.filterText
        ++ maybe [] (\value -> [("group", Just (cs value))]) f.filterGroup
        ++ [("sort", Just (cs f.filterSort)), ("dir", Just (cs f.filterDir))]
        ++ map (\col -> ("cols", Just (cs col))) f.filterCols
        ++ [("pageSize", Just (cs (tshow f.filterPageSize)))]
        ++ maybe [] (\value -> [("occ_min", Just (cs (tshow value)))]) f.filterOccMin
        ++ maybe [] (\value -> [("seen", Just (cs value))]) f.filterSeenWithin
        ++ [("view", Just (cs viewMode))]

envPrefsAreDefault :: (EnvFilters, Text) -> Bool
envPrefsAreDefault (filters, viewMode) = filters == emptyEnvFilters && viewMode == "flat"

data ShowView = ShowView
    { environmentName :: Text
    , environment :: Maybe Environment
    , alerts :: [Alert]
    , groups :: [(AlertGroup, [Alert])]
    , blackouts :: [Blackout]
    , filters :: EnvFilters
    , viewMode :: Text
    , total :: Int64
    , groupKeys :: [(Id AlertGroup, Text)]
    }

instance View ShowView where
    html ShowView{..} =
        [hsx|
        <div data-live-scope={"env:" <> environmentName} data-live-filters={liveFilters}>
            <h1>{environmentName}</h1>
            {activeBlackoutNotice}
            <div class="mb-2" data-testid="view-toggle">
                <a href={toggleUrl "flat"} class={toggleClass "flat"} data-testid="view-flat">Flat</a>
                <a href={toggleUrl "grouped"} class={toggleClass "grouped"} data-testid="view-grouped">Grouped</a>
            </div>
            {content}
        </div>
    |]
      where
        -- Same staleness caveat as the /alerts table (see Alerts.Index):
        -- the client subscribes with THESE filters, not location.search.
        liveFilters :: Text
        liveFilters = cs (Aeson.encode (envFiltersToValue filters viewMode))
        activeBlackoutNotice =
            if null blackouts
                then mempty
                else
                    [hsx|
                    <div class="alert alert-secondary blackout-notice" data-testid="blackout-notice">
                        Blackout active — new alerts are suppressed.
                    </div>
                |]
        toggleUrl mode = pathTo (ShowEnvironmentAction environmentName) <> cs (renderQuery True (envBaseItems filters mode))
        toggleClass :: Text -> Text
        toggleClass mode = if viewMode == mode then "btn btn-sm btn-secondary" else "btn btn-sm btn-outline-secondary"
        content =
            if viewMode == "grouped"
                then groupedAlertsTableHtml (Just "env-groups-table") "env-groups-tbody" groups
                else flatTable
        flatTable =
            dynTableHtml
                DynTable
                    { dtTestId = Just "env-alerts-table"
                    , dtTbodyId = "env-alerts-tbody"
                    , -- The wrapper div carries the live scope/filters.
                      dtLiveScope = Nothing
                    , dtLiveFilters = Nothing
                    , dtTableClass = "table"
                    , dtConfig = tableConfig
                    , dtState = tableState
                    , dtBasePath = pathTo (ShowEnvironmentAction environmentName)
                    , dtResetUrl = Just resetUrl
                    , dtExtraItems = [("view", Just (cs viewMode))]
                    , dtTotal = total
                    , dtRows = alerts
                    , dtRowHtml = rowHtml
                    }
        rowHtml visible alert =
            alertRowHtmlCols (alert.groupId >>= (`lookup` groupKeys)) (map colKey visible) alert
        resetUrl :: Text
        resetUrl = pathTo (ShowEnvironmentAction environmentName) <> "?reset=1"
        tableConfig =
            TableConfig
                { cfgName = "env"
                , cfgColumns = alertListColumns Nothing alerts
                , cfgDefaultVisible = defaultAlertListColumns
                , cfgDefaultSort = "last_seen_at"
                , cfgDefaultDir = "desc"
                , cfgPageSizes = alertListPageSizes
                , cfgDefaultPageSize = defaultAlertListPageSize
                , cfgColumnPicker = True
                , cfgPager = True
                }
        tableState =
            TableState
                { tsSort = filters.filterSort
                , tsDir = filters.filterDir
                , tsPage = filters.filterPage
                , tsPageSize = filters.filterPageSize
                , tsVisible = filters.filterCols
                , tsFilters =
                    [("severity", filters.filterSeverities), ("status", filters.filterStatuses)]
                        ++ single "host" filters.filterHost
                        ++ single "service" filters.filterService
                        ++ single "q" filters.filterText
                        ++ single "group" filters.filterGroup
                        ++ single "occ_min" (tshow <$> filters.filterOccMin)
                        ++ single "seen" filters.filterSeenWithin
                }
        single param = maybe [] (\value -> [(param, [value])])
