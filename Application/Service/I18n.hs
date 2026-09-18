module Application.Service.I18n (
    Language (..),
    languages,
    isValidLanguage,
    languageCode,
    languageName,
    languageFromCode,
    languageFromSettings,
    defaultLanguage,
    defaultLanguageName,
    agentLanguageName,
    translate,
    translateParams,
) where

import Application.Service.I18n.CatalogRu (catalogRu)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import IHP.Prelude
import System.Environment (lookupEnv)

-- UI/prompt internationalization. IHP has no i18n layer, so translations are
-- gettext-style: the English source string is the lookup key, missing entries
-- fall back to English. The desired language persists in
-- users.settings.language (profile page); LLM prompt templates get the
-- language through the {{language}} slot (Application.Service.Llm.Prompt).
-- Request-context helpers (tr/trp/currentLanguage) live in
-- Application.Helper.I18n to keep this module usable from worker jobs.

data Language = LangEn | LangRu
    deriving (Eq, Show)

-- (code, native label) pairs for the profile selector.
languages :: [(Text, Text)]
languages = [("en", "English"), ("ru", "Русский")]

isValidLanguage :: Text -> Bool
isValidLanguage = isJust . languageFromCode

languageCode :: Language -> Text
languageCode LangEn = "en"
languageCode LangRu = "ru"

-- Human-readable name injected into LLM prompts via {{language}}.
languageName :: Language -> Text
languageName LangEn = "English"
languageName LangRu = "Russian"

languageFromCode :: Text -> Maybe Language
languageFromCode "en" = Just LangEn
languageFromCode "ru" = Just LangRu
languageFromCode _ = Nothing

languageFromSettings :: Value -> Language
languageFromSettings settings =
    case parseMaybe (Aeson.withObject "settings" (\o -> o Aeson..: "language")) settings of
        Just code | Just language <- languageFromCode code -> language
        _ -> LangEn

-- System-wide default for contexts without a user (worker jobs):
-- HALEMANS_DEFAULT_LANGUAGE env var, "en" when unset/unknown.
defaultLanguage :: IO Language
defaultLanguage = do
    value <- lookupEnv "HALEMANS_DEFAULT_LANGUAGE"
    pure (fromMaybe LangEn (languageFromCode . cs =<< value))

defaultLanguageName :: IO Text
defaultLanguageName = languageName <$> defaultLanguage

-- Prompt language for an LLM analysis: the queueing user's profile language
-- stamped on the analysis row (llm_analyses.language), else the system
-- default. Worker jobs have no request context of their own.
agentLanguageName :: Maybe Text -> IO Text
agentLanguageName (Just code) | Just language <- languageFromCode code = pure (languageName language)
agentLanguageName _ = defaultLanguageName

translate :: Language -> Text -> Text
translate LangEn key = key
translate LangRu key = fromMaybe key (Map.lookup key catalogRu)

-- Placeholder variant for dynamic messages: {name} markers are substituted
-- AFTER translation so Russian word order can move them freely.
translateParams :: Language -> Text -> [(Text, Text)] -> Text
translateParams language key params = foldl' step (translate language key) params
  where
    step acc (name, value) = Text.replace ("{" <> name <> "}") value acc
