module Lib
    ( entry
    ) where

import           Ignition.Plug
import           Ignition.Spark         (allSparks, fromString, key)

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Char              as C
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           System.Environment     (getArgs)

entry :: IO ()
entry = do
    sparkArgs <- validateSparks
    let sparks = fromString <$> sparkArgs
        output = T.unpack $ ignite sparks

    writeFile "./Vagrantfile" output

supportedSparks :: [String]
supportedSparks = map toLowerStr sparkStrings
  where
    sparkStrings = map (show . key) allSparks
    toLowerStr   = map C.toLower

unsupportedSpark :: String -> Bool
unsupportedSpark spark = spark `notElem` supportedSparks

validateSparks :: IO [String]
validateSparks = do
    sparkArgs <- getArgs

    when (any unsupportedSpark sparkArgs) $
        liftIO $ fail unsupportedMessage

    if "base" `notElem` sparkArgs
        then return ("base" : sparkArgs)
        else return sparkArgs

  where
    unsupportedMessage = "Supported sparks: " <> unwords supportedSparks
