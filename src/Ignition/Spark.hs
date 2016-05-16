{-# LANGUAGE QuasiQuotes #-}

module Ignition.Spark
    ( SparkKey
    , Spark
    , fromString
    , ignite
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import Text.Heredoc
import Ignition.OS

data SparkKey = Base | Postgres | Haskell | Elixir | Java | Clojure |
                Ruby | Node | Elm deriving (Show)

data Spark = Spark {
    sparkKey   :: SparkKey,
    sparkDeps  :: [Spark],
    sparkRoot  :: Bool,
    sparkValue :: Text
} deriving (Show)

fromString :: String -> Spark
fromString x = case x of
    "base"     -> base
    "postgres" -> postgres
    "haskell"  -> haskell
    "elixir"   -> elixir
    "java"     -> java
    "clojure"  -> clojure
    "ruby"     -> ruby
    "node"     -> node
    "elm"      -> elm
    _          -> base

ignite :: [Spark] -> Text
ignite sparks = T.concat (sparkHeader <$> sparks) <> boilerplate <> box <> T.concat (sparkString <$> sparks) <> "end\n"
  where boilerplate = [str|
                          |Vagrant.configure(2) do |config|
                          |  config.vm.synced_folder ".", "/vagrant", smb_username: ENV['SMB_USERNAME'], smb_password: ENV['SMB_PASSWORD']
                          |]

sparkHeader :: Spark -> Text
sparkHeader spark = T.concat (sparkHeader <$> sparkDeps spark) <> shellScript spark

sparkString :: Spark -> Text
sparkString spark = T.concat (sparkString <$> sparkDeps spark) <> shellProv spark

shellScript :: Spark -> Text
shellScript spark = shelldoc
  where name     = T.pack $ show $ sparkKey spark
        value    = sparkValue spark
        shelldoc = name <> " = <<-SHELL" <> value <> "SHELL" <> "\n\n"

shellProv :: Spark -> Text
shellProv spark = cmd
  where name = T.pack $ show (sparkKey spark)
        root = T.pack $ toLower <$> show (sparkRoot spark)
        cmd  = "  config.vm.provision :shell, inline: " <> name <> ", privileged: " <> root <> "\n"

box :: Text
box = if isWindows
      then winBox
      else other
  where winBox = [str|
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

base :: Spark
base = Spark Base [] True $ if isWindows
       then baseCode
       else baseCode <> vbUtils
  where vbUtils  = "apt-get install -y virtualbox-guest-utils\n"
        baseCode = [str|
                       |# Stack (Haskell) repo
                       |apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
                       |echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list
                       |
                       |# Elixir (Erlang) repo
                       |wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb | dpkg -i
                       |
                       |# Clojure (Java)
                       |add-apt-repository ppa:webupd8team/java
                       |
                       |# Elm, JS-whatever (Node) repo
                       |###### Forces update
                       |curl -sL https://deb.nodesource.com/setup_6.x | bash -
                       |
                       |apt-get upgrade -y
                       |apt-get install -y build-essential git gnupg curl wget
                       |]

postgres :: Spark
postgres = Spark Postgres [] True [str|
               |apt-get install -y postgresql libpq-dev
               |sudo -u postgres psql -U postgres -d postgres -c "alter user postgres with password 'postgres';"
               |]
haskell :: Spark
haskell = Spark Haskell [] True [str|
                                    |apt-get install -y stack
                                    |]

elixir :: Spark
elixir = Spark Elixir [] True [str|
                                  |apt-get install -y esl-erlang elixir
                                  |]

java :: Spark
java = Spark Java [] True [str|
           |echo "debconf shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
           |echo "debconf shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections
           |apt-get install -y oracle-java8-installer
           |]

clojure :: Spark
clojure = Spark Clojure [java] False [str|
              |wget -P ~/bin/ https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
              |chmod a+x ~/bin/lein
              |export PATH=$HOME/bin:$PATH
              |lein
              |]

ruby :: Spark
ruby = Spark Ruby [] False [str|
           |gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
           |\curl -sSL https://get.rvm.io | bash -s stable
           |source ~/.rvm/scripts/rvm
           |
           |echo "rvm_install_on_use_flag=1" >> ~/.rvmrc
           |
           |cd /vagrant
           |]

node :: Spark
node = Spark Node [] True [str|
           |apt-get install -y nodejs
           |npm update -g npm
           |]

elm :: Spark
elm = Spark Elm [node] True [str|
                                |npm install -g elm
                                |]
