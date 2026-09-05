module Application.Service.Llm.Output
( ParsedOutput (..)
, parseCompletionOutput
, outputContract
) where

import IHP.Prelude
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- Structured-output convention (design_docs/milestone_4.md §5, D5): the model
-- answers with markdown analysis followed by a ```json fenced block carrying
-- probable_cause/confidence/suggested_actions/references. The contract text
-- is appended by the prompt builder, not stored in the DB template.
--
-- Parsing is best-effort: a missing or malformed fence degrades to a
-- markdown-only result and must never crash the card.

data ParsedOutput = ParsedOutput
    { markdown :: Text
    , structured :: Maybe Value
    } deriving (Eq, Show)

parseCompletionOutput :: Text -> ParsedOutput
parseCompletionOutput raw =
    let (before, rest) = break (Text.isPrefixOf "```json" . Text.strip) (Text.lines raw)
    in case rest of
        [] -> markdownOnly
        (_fence:inside) ->
            let (jsonLines, _) = break ((== "```") . Text.strip) inside
                decoded = Aeson.decode (cs (Text.unlines jsonLines)) :: Maybe Value
            in case decoded of
                Just value@(Aeson.Object _) -> ParsedOutput
                    { markdown = Text.strip (Text.unlines before)
                    , structured = Just value
                    }
                _ -> markdownOnly
    where
        markdownOnly = ParsedOutput { markdown = Text.strip raw, structured = Nothing }

outputContract :: Text
outputContract = Text.intercalate "\n"
    [ ""
    , "Respond with a markdown analysis of this alert for an on-call engineer."
    , "After the markdown, append exactly one ```json fenced block with this shape:"
    , "{\"probable_cause\": string, \"confidence\": number between 0 and 1,"
    , " \"suggested_actions\": [string], \"references\": [string]}"
    ]
