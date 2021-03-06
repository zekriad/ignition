module Ignition.OS
    ( isWindows
    ) where

import           Data.Text   (Text, isInfixOf, pack)
import           System.Info (os)

isWindows :: Bool
isWindows = any isWindowsKey keys
  where keys = ["mswin", "msys", "mingw", "cygwin", "bccwin", "wince", "emc"]

isWindowsKey :: Text -> Bool
isWindowsKey str = str `isInfixOf` pack os
