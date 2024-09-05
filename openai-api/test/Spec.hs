import ApiSpec
import Control.Monad
import HelperSpec
import System.Environment
import Test.Hspec

isCI :: IO Bool
isCI = maybe False (const True) <$> lookupEnv "CI"

main :: IO ()
main = do
  runningInCI <- isCI
  hspec $
    do
      unless runningInCI apiSpec
      helperSpec
