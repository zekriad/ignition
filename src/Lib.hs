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
    let sparks = fromString <$> sparkArgs
        output = T.unpack $ ignite sparks

    writeFile "./Vagrantfile" output

supportedSparks :: [String]
supportedSparks = ["base", "postgres", "haskell", "elixir", "java", "clojure", "ruby", "node", "elm"]

unsupportedSpark :: String -> Bool
unsupportedSpark spark = spark `notElem` supportedSparks

validateSparks :: IO [String]
validateSparks = do
    sparkArgs <- getArgs

    when (any unsupportedSpark sparkArgs) $
        liftIO $ fail unsupportedMessage

    let fooArgs = if "base" `notElem` sparkArgs
        then "base" : sparkArgs
        else sparkArgs

    return fooArgs

  where
    unsupportedMessage = "Supported sparks: " <> unwords supportedSparks
