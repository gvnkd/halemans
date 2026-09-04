module Application.Helper.DashboardConfig
( DashboardCard (..)
, decodeDashboardConfig
, encodeDashboardConfig
, renderDashboardConfig
) where

import IHP.Prelude
import Data.Aeson (Value, object, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)

-- User dashboard config (design_docs/milestone_3.md §7): an ordered list of
-- {env, filters} cards stored in dashboards.config (and, as a template, in
-- teams.default_dashboard_config). Order in the array is display order.

data DashboardCard = DashboardCard
    { cardEnv :: Text
    , cardStatuses :: [Text]
    , cardSeverities :: [Text]
    } deriving (Eq, Show)

instance Aeson.FromJSON DashboardCard where
    parseJSON = Aeson.withObject "DashboardCard" \o -> do
        cardEnv <- o .: "env"
        filtersValue <- o .:? "filters"
        let filters = case filtersValue of
                Just (Aeson.Object f) -> f
                _ -> KeyMap.empty
        cardStatuses <- filters .:? "status" .!= []
        cardSeverities <- filters .:? "severity" .!= []
        pure DashboardCard { .. }

instance Aeson.ToJSON DashboardCard where
    toJSON card = object
        [ "env" .= card.cardEnv
        , "filters" .= object
            [ "status" .= card.cardStatuses
            , "severity" .= card.cardSeverities
            ]
        ]

decodeDashboardConfig :: Value -> Either Text [DashboardCard]
decodeDashboardConfig value = case parseEither Aeson.parseJSON value of
    Left err -> Left (cs err)
    Right cards -> Right cards

encodeDashboardConfig :: [DashboardCard] -> Value
encodeDashboardConfig = Aeson.toJSON

-- | Pretty-printed JSON for the edit form textarea.
renderDashboardConfig :: [DashboardCard] -> Text
renderDashboardConfig = cs . Aeson.encode
