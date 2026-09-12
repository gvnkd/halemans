module Web.Controller.Metrics where

import Application.Service.Api.Auth (withApiToken)
import Application.Service.Api.Metrics (collectMetrics)
import Application.Service.Api.RateLimit (LimitClass (..))
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Web.Controller.Prelude

-- Prometheus scrape endpoint (design_docs/milestone_6.md §5): token-gated
-- with the metrics scope, no session-cookie path.
instance Controller MetricsController where
    action MetricsAction = withApiToken LimitMetrics "metrics" \_ _ -> do
        body <- collectMetrics
        respondAndExit $
            responseLBS
                status200
                [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")]
                (cs body)
