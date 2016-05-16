module Lib
    ( entry
    ) where

import System.Environment (getArgs)
import Data.Monoid ((<>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Ignition.Spark
import qualified Data.Text as T

entry :: IO ()
entry = do
    sparkArgs <- validateSparks
    let sparks = map fromString sparkArgs

    writeFile "./Vagrantfile" $ T.unpack $ ignite sparks

supportedSparks :: [String]
supportedSparks = ["base", "postgres", "haskell", "elixir", "java", "clojure", "ruby", "node", "elm"]

unsupportedSpark :: String -> Bool
unsupportedSpark spark = spark `notElem` supportedSparks

validateSparks :: IO [String]
validateSparks = do
    sparkArgs <- getArgs

    when (any unsupportedSpark sparkArgs) $
        liftIO $ fail unsupportedMessage

    return sparkArgs

  where
    unsupportedMessage = "Supported sparks: " <> unwords supportedSparks
