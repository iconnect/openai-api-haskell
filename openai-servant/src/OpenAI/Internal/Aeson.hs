-- |
module OpenAI.Internal.Aeson (jsonOpts, jsonEnumsOpts, deriveJSON, ToJSON, FromJSON) where

import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import Text.Casing (quietSnake)

jsonOpts :: Int -> Options
jsonOpts x =
  defaultOptions
    { fieldLabelModifier = quietSnake . drop x,
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
