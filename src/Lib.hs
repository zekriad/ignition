module Lib
    ( entry
    ) where

import System.Environment (getArgs)
import Network.Wreq
import Control.Lens((^.))
import qualified Data.ByteString.Lazy.Char8 as BLC (unpack)
import Data.Monoid ((<>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

entry :: IO ()
entry = do
    response <- get "https://raw.githubusercontent.com/zekriad/ignition/master/template/Vagrantfile"
    stacks   <- stackString

    let template    = response ^. responseBody
        vagrantfile = stackLine stacks <> BLC.unpack template

    writeFile "./Vagrantfile" vagrantfile

supportedStacks :: [String]
supportedStacks = ["base", "postgres", "haskell", "elixir", "java", "clojure", "ruby", "node", "elm"]

unsupportedStack :: String -> Bool
unsupportedStack stack = stack `notElem` supportedStacks

stackString :: IO String
stackString = do
    stackArgs <- getArgs

    when (any unsupportedStack stackArgs) $
        liftIO $ fail unsupportedMessage

    return $ unwords stackArgs

  where
    unsupportedMessage = "Supported stacks: " <> unwords supportedStacks

stackLine :: String -> String
stackLine stacks = "PLATFORMS = %i(" <> stacks <> ")\n\n"
