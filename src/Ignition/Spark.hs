{-# LANGUAGE QuasiQuotes #-}

module Ignition.Spark
    ( Spark
    , SparkKey
    , key
    , dependencies
    , needsRoot
    , provision
    , allSparks
    , fromString
    ) where

import           Ignition.OS

import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import           Text.Heredoc (str)

data SparkKey = Base | Postgres| Redis | Haskell | Elixir | Java |
                Clojure | Ruby | Node | Elm | Rails deriving (Show)

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
        "rails"    -> rails
        _          -> base

allSparks :: [Spark]
allSparks = [base, postgres, redis, haskell, elixir, java, clojure, ruby, node, elm, rails]

base :: Spark
base = Spark Base [] True $ if isWindows
       then baseCode
       else baseCode <> vbUtils
  where
    vbUtils  = "apt-get install -qq -y virtualbox-guest-utils\n"
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
                   |apt-get upgrade -qq -y
                   |apt-get install -qq -y build-essential git gnupg curl
                   |]

postgres :: Spark
postgres = Spark Postgres [] True [str|
    |apt-get install -qq -y postgresql libpq-dev
    |sudo -u postgres psql -U postgres -d postgres -c "alter user postgres with password 'postgres';"
    |sed -i '/local\s*all\s*postgres\s*peer/ s/peer/md5/g' /etc/postgresql/9.5/main/pg_hba.conf
    |service postgresql restart
    |]

redis :: Spark
redis = Spark Redis [] True [str|
    |apt-get install -qq -y redis-server
    |]

haskell :: Spark
haskell = Spark Haskell [] True [str|
    |apt-get install -qq -y stack
    |]

elixir :: Spark
elixir = Spark Elixir [] True [str|
    |apt-get install -qq -y esl-erlang elixir
    |]

java :: Spark
java = Spark Java [] True [str|
    |echo "debconf shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
    |echo "debconf shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections
    |apt-get install -qq -y oracle-java8-installer
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
    |curl -sSL https://get.rvm.io | bash -s stable
    |source ~/.rvm/scripts/rvm
    |
    |echo "rvm_install_on_use_flag=1" >> ~/.rvmrc
    |
    |cd /vagrant
    |]

node :: Spark
node = Spark Node [] True [str|
    |apt-get install -qq -y nodejs
    |npm update -g --quiet npm
    |]

elm :: Spark
elm = Spark Elm [node] True [str|
    |npm install -g --quiet elm
    |]

rails :: Spark
rails = Spark Rails [postgres, node, ruby] False [str|
    |cd /vagrant
    |gem install bundler
    |bundle install
    |]
