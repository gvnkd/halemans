module Application.Service.TimeRange (
    resolveTimeExpr,
) where

import Data.Char (isDigit)
import qualified Data.Text as Text
import Data.Time
import IHP.Prelude
import Text.Read (readMaybe)

-- Time range expressions for the /reports from/to fields. Either a relative
-- offset from now ("now()", "now() - 7d", "now()+12h"; units s/m/h/d/w,
-- whitespace-tolerant, case-insensitive) or an absolute timestamp. Absolute
-- values with an explicit zone (Z / +hh:mm) are honored; zone-less values
-- are read as UTC (browser JS converts local datetimes to UTC ISO before
-- submit, so zone-less input only appears when JS is off).
resolveTimeExpr :: UTCTime -> Text -> Maybe UTCTime
resolveTimeExpr now expr =
    case Text.strip (Text.toLower expr) of
        lowered | Just rest <- Text.stripPrefix "now()" lowered -> applyOffset (Text.strip rest)
        _ -> parseAbsolute (Text.strip expr)
  where
    applyOffset "" = Just now
    applyOffset rest = case Text.uncons rest of
        Just ('-', amount) -> addUTCTime . negate <$> parseAmount amount <*> pure now
        Just ('+', amount) -> addUTCTime <$> parseAmount amount <*> pure now
        _ -> Nothing

parseAmount :: Text -> Maybe NominalDiffTime
parseAmount raw = do
    let (digits, unitRaw) = Text.span isDigit (Text.strip raw)
        unit = Text.strip unitRaw
    n <- readMaybe (cs digits) :: Maybe Integer
    factor <- lookup unit [("s", 1), ("m", 60), ("h", 3600), ("d", 86400), ("w", 604800)]
    pure (fromIntegral (n * factor))

parseAbsolute :: Text -> Maybe UTCTime
parseAbsolute value = tryFormat formats
  where
    tryFormat = foldr (\fmt acc -> parse fmt <|> acc) Nothing
    parse fmt = parseTimeM True defaultTimeLocale fmt (cs value)
    formats =
        [ "%Y-%m-%dT%H:%M:%S%QZ"
        , "%Y-%m-%dT%H:%M:%SZ"
        , "%Y-%m-%dT%H:%M:%S%Q%z"
        , "%Y-%m-%dT%H:%M:%S%z"
        , "%Y-%m-%d %H:%M:%S"
        , "%Y-%m-%d %H:%M"
        , "%Y-%m-%d"
        ]
