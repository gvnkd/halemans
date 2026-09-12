module Application.Helper.FilterPrefs (
    filterPrefsFor,
    saveFilterPrefs,
    clearFilterPrefs,
    hasQueryKeys,
) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Generated.Types
import IHP.ModelSupport (ModelContext, updateRecord)
import IHP.Prelude
import Network.Wai (Request, queryString)

-- Per-user persisted filter state lives under settings.filters.<name>
-- (name = "alerts" | "env"); written on every explicit filter submission,
-- re-applied on bare page visits, removed only via the Reset link.
filterPrefsFor :: Aeson.Value -> Text -> Maybe Aeson.Value
filterPrefsFor settings name = case settings of
    Aeson.Object o -> case KeyMap.lookup "filters" o of
        Just (Aeson.Object filters) -> KeyMap.lookup (Key.fromText name) filters
        _ -> Nothing
    _ -> Nothing

saveFilterPrefs :: (?modelContext :: ModelContext) => User -> Text -> Aeson.Value -> IO ()
saveFilterPrefs user name value = void do
    user
        |> set #settings (withFilters (KeyMap.insert (Key.fromText name) value) user.settings)
        |> updateRecord

clearFilterPrefs :: (?modelContext :: ModelContext) => User -> Text -> IO ()
clearFilterPrefs user name = void do
    user
        |> set #settings (withFilters (KeyMap.delete (Key.fromText name)) user.settings)
        |> updateRecord

withFilters :: (KeyMap.KeyMap Aeson.Value -> KeyMap.KeyMap Aeson.Value) -> Aeson.Value -> Aeson.Value
withFilters f settings =
    let (outer, filters) = case settings of
            Aeson.Object o ->
                ( o
                , case KeyMap.lookup "filters" o of
                    Just (Aeson.Object existing) -> existing
                    _ -> KeyMap.empty
                )
            _ -> (KeyMap.empty, KeyMap.empty)
     in Aeson.Object (KeyMap.insert "filters" (Aeson.Object (f filters)) outer)

-- Distinguishes an explicit filter form submission (all known keys are
-- always submitted, even empty) from a bare page visit (no query string).
hasQueryKeys :: [ByteString] -> Request -> Bool
hasQueryKeys keys request = any (`elem` keys) (map fst (queryString request))
