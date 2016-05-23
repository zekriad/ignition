{-# LANGUAGE QuasiQuotes #-}

module Ignition.Plug
    ( ignite
    ) where

import           Ignition.OS
import           Ignition.Spark

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Text.Heredoc   (str)

concatMap' :: (a -> Text) -> [a] -> Text
concatMap' f xs = T.concat (f <$> xs)

showText :: Show a =>  a -> Text
showText = T.pack . show

indent :: Text -> Text
indent = T.unlines . map addSpace . T.lines
  where
    addSpace t = "  " <> t

ignite :: [Spark] -> Text
ignite sparks = boilerplate <> header <> box <> config <> "end\n"
  where
    header      = concatMap' sparkScript sparks
    config      = concatMap' sparkProv sparks
    boilerplate = [str|
                      |Vagrant.configure(2) do |config|
                      |  config.vm.synced_folder ".", "/vagrant", smb_username: ENV['SMB_USERNAME'], smb_password: ENV['SMB_PASSWORD']
                      |]

sparkScript :: Spark -> Text
sparkScript spark = concatMap' sparkScript deps <> script
  where
    deps   = dependencies spark
    script = sparkScript' spark

sparkScript' :: Spark -> Text
sparkScript' spark = shelldoc
  where
    name     = showText (key spark)
    value    = provision spark
    shelldoc = indent $ "\n" <> T.toLower name <> " = <<-SHELL" <> value <> "SHELL" <> "\n"

sparkProv :: Spark -> Text
sparkProv spark = concatMap' sparkProv deps <> prov
  where
    deps = dependencies spark
    prov = sparkProv' spark

sparkProv' :: Spark -> Text
sparkProv' spark = cmd
  where
    name = showText (key spark)
    root = T.toLower . showText $ needsRoot spark
    cmd  = indent $ "config.vm.provision :shell, inline: " <> T.toLower name <> ", privileged: " <> root <> "\n"

box :: Text
box = if isWindows
      then winBox
      else other
  where
    winBox = [str|
                 |  config.vm.box = "nikel/xerus64"
                 |  config.vm.provider :hyperv do |hyperv|
                 |    hyperv.vmname = "#{File.dirname(__FILE__)}-vagrant"
                 |    hyperv.memory = "1024"
                 |  end
                 |
                 |]
    other  = [str|
                 |  config.vm.box = "ubuntu/xenial64"
                 |  config.vm.provider :virtualbox do |vb|
                 |    # vb.name   = "#{File.dirname(__FILE__)}-vagrant" # virtualbox copy error on OS X; looks like vagrant calls incorrectly
                 |    vb.memory = "1024"
                 |    vb.gui    = false
                 |  end
                 |
                 |]
