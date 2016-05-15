module Ignition.OS where

import System.Info (os)
import Data.Text (isInfixOf, Text, pack)

isWindows :: Bool
isWindows = any isWindowsKey windowsKeys
  where windowsKeys = ["mswin", "msys", "mingw", "cygwin", "bccwin", "wince", "emc"]


isWindowsKey :: Text -> Bool
isWindowsKey str = str `isInfixOf` pack os
