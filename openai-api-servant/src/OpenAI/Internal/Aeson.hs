-- |
module OpenAI.Internal.Aeson (jsonOpts, slashify, jsonOptsSlashSeparated, jsonEnumsOpts, deriveJSON, ToJSON, FromJSON) where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Text.Casing
import Data.Char
import Data.List (intersperse)
import qualified Data.Text as T

jsonOpts :: Int -> Options
jsonOpts x =
  defaultOptions
    { fieldLabelModifier = quietSnake . drop x,
      constructorTagModifier = quietSnake,
      omitNothingFields = True
    }

-- | Like the kebab case, but with a different separator.
slashify :: String -> String
slashify = T.unpack . T.replace "$/" "-"
                    . T.pack
                    . concat
                    . intersperse "/"
                    . map (map toLower)
                    . unIdentifier
                    . fromAny
                    . T.unpack
                    . T.replace "_" "$"
                    . T.pack

-- | Useful for things like moderation categories
-- mrFooBaz
-- > "foo/baz"
jsonOptsSlashSeparated :: Int -> Options
jsonOptsSlashSeparated x =
  defaultOptions
    { fieldLabelModifier = slashify . drop x,
      constructorTagModifier = quietSnake,
      omitNothingFields = True
    }

jsonEnumsOpts :: Int -> Options
jsonEnumsOpts x =
  defaultOptions
    { fieldLabelModifier = quietSnake . drop x,
      constructorTagModifier = quietSnake . drop x,
      omitNothingFields = True
    }
