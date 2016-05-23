{-# LANGUAGE QuasiQuotes #-}

module Ignition.Spark
    ( Spark
    , SparkKey
    , key
    , allSparks
    , fromString
    , ignite
    ) where

import           Ignition.OS

import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Text.Heredoc (str)

data SparkKey = Base | Postgres| Redis | Haskell | Elixir | Java |
                Clojure | Ruby | Node | Elm deriving (Show)

data Spark = Spark
    { key          :: SparkKey
    , dependencies :: [Spark]
    , needsRoot    :: Bool
    , provision    :: Text
    } deriving (Show)

fromString :: String -> Spark
fromString x =
    case x of
        "base"     -> base
        "postgres" -> postgres
        "redis"    -> redis
        "haskell"  -> haskell
        "elixir"   -> elixir
        "java"     -> java
        "clojure"  -> clojure
        "ruby"     -> ruby
        "node"     -> node
        "elm"      -> elm
        _          -> base

allSparks :: [Spark]
allSparks = [base, postgres, redis, haskell, elixir, java, clojure, ruby, node, elm]

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

base :: Spark
base = Spark Base [] True $ if isWindows
       then baseCode
       else baseCode <> vbUtils
  where
    vbUtils  = "apt-get install -q -y virtualbox-guest-utils\n"
    baseCode = [str|
                   |# Redis
                   |add-apt-repository ppa:chris-lea/redis-server
                   |
                   |# Stack (Haskell) repo
                   |apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
                   |echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list
                   |
                   |# Elixir (Erlang) repo
                   |wget --quiet https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
                   |dpkg -i erlang-solutions_1.0_all.deb
                   |rm erlang-solutions_1.0_all.deb
                   |
                   |# Clojure (Java)
                   |add-apt-repository ppa:webupd8team/java
                   |
                   |# Elm, JS-whatever (Node) repo
                   |###### Forces update
                   |curl -sL https://deb.nodesource.com/setup_6.x | bash -
                   |
                   |apt-get upgrade -q -y
                   |apt-get install -q -y build-essential git gnupg curl
                   |]

postgres :: Spark
postgres = Spark Postgres [] True [str|
    |apt-get install -q -y postgresql libpq-dev
    |sudo -u postgres psql -U postgres -d postgres -c "alter user postgres with password 'postgres';"
    |]

redis :: Spark
redis = Spark Redis [] True [str|
    |apt-get install -q -y redis-server
    |]

haskell :: Spark
haskell = Spark Haskell [] True [str|
    |apt-get install -q -y stack
    |]

elixir :: Spark
elixir = Spark Elixir [] True [str|
    |apt-get install -q -y esl-erlang elixir
    |]

java :: Spark
java = Spark Java [] True [str|
    |echo "debconf shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
    |echo "debconf shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections
    |apt-get install -q -y oracle-java8-installer
    |]

clojure :: Spark
clojure = Spark Clojure [java] False [str|
    |wget --quiet -P ~/bin/ https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
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
    |apt-get install -q -y nodejs
    |npm update -g --quiet npm
    |]

elm :: Spark
elm = Spark Elm [node] True [str|
    |npm install -g --quiet elm
    |]
