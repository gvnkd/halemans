module Application.Service.Facets (
    resolveFacets,
    facetsToJSON,
    facetValue,
    computeFacetsValue,
    computeFacetsValueWithLinks,
    materializeFacets,
) where

import Application.Pipeline.Grouping (alertFieldText, facetValue, labelValue, parseAlertField)
import Application.Service.Assets.Attrs (configuredAttrNames, objectAttributes)
import qualified Application.Service.Assets.Cache as AssetsCache
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import qualified Data.Text as Text
import Generated.Types
import IHP.Fetch (fetch)
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder (filterWhere, orderByAsc, query)

-- Resolved facets (design_docs/milestone_9.md §3): a flattened facet-name →
-- value map materialized on alerts.facets. Pure core (resolveFacets) never
-- touches the DB; the shell functions fetch mappings/configs/links.

-- | For each facet, walk its mappings by ascending rank; first kind that
-- yields a non-empty value wins. Additionally every whitelisted Assets
-- attribute (attrNames, from enabled configs' attribute_names) is copied
-- verbatim; mapping-derived facets win on key collision.
resolveFacets :: [FieldMapping] -> [Text] -> [AssetsObject] -> Alert -> [(Text, Text)]
resolveFacets mappings attrNames objects alert = mapped ++ verbatim
  where
    mapped = concatMap resolveFacet byFacet
    byFacet = groupBy ((==) `on` (.facet)) (sortOn (\m -> (m.facet, m.rank)) enabled)
    enabled = filter (.enabled) mappings
    resolveFacet ms = case catMaybes (map applyMapping ms) of
        (value : _) -> maybe [] (\m -> [(m.facet, value)]) (head ms)
        [] -> []
    applyMapping mapping =
        nonEmpty =<< case mapping.kind of
            "field" -> case parseAlertField mapping.key of
                Just field -> alertFieldText field alert
                Nothing -> Nothing
            "label" -> labelValue alert mapping.key
            "attr" -> firstCsvElement =<< attrFromObjects mapping.key
            _ -> Nothing
    attrFromObjects name = head (catMaybes [lookup name (objectAttributes object) | object <- objects])
    -- Assets list-typed attributes arrive as a comma-separated string;
    -- mapping purposes use the first element only.
    firstCsvElement value = Text.strip <$> head (Text.splitOn "," value)
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value
    mappedKeys = map fst mapped
    verbatim =
        nub
            [ (name, value)
            | object <- objects
            , name <- attrNames
            , name `notElem` mappedKeys
            , Just value <- [nonEmpty =<< lookup name (objectAttributes object)]
            ]

facetsToJSON :: [(Text, Text)] -> Value
facetsToJSON pairs = Aeson.object [Key.fromText name Aeson..= value | (name, value) <- pairs]

-- | DB shell: resolve facets for an alert against a given object list
-- (empty at ingest, linked objects after enrichment).
computeFacetsValue :: (?modelContext :: ModelContext) => [AssetsObject] -> Alert -> IO Value
computeFacetsValue objects alert = do
    mappings <-
        query @FieldMapping
            |> orderByAsc #rank
            |> fetch
    configs <-
        query @AssetsConfig
            |> filterWhere (#enabled, True)
            |> fetch
    let attrNames = concatMap configuredAttrNames configs
    pure (facetsToJSON (resolveFacets mappings attrNames objects alert))

-- | Resolve using the alert's currently linked assets objects.
computeFacetsValueWithLinks :: (?modelContext :: ModelContext) => Alert -> IO Value
computeFacetsValueWithLinks alert = do
    linked <- AssetsCache.linkedAssetsForAlert alert
    computeFacetsValue (map snd linked) alert

-- | Re-resolve facets from linked objects and persist them on the alert row.
materializeFacets :: (?modelContext :: ModelContext) => Alert -> IO Alert
materializeFacets alert = do
    facetsValue <- computeFacetsValueWithLinks alert
    alert
        |> set #facets facetsValue
        |> updateRecord
