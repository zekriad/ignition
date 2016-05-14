# -*- mode: ruby -*-
# vi: set ft=ruby :

PLATFORMS = %i(base) # %i(base postgres haskell elixir java clojure ruby node elm)

def os
 @os ||= (
   case RUBY_PLATFORM
   when /mswin|msys|mingw|cygwin|bccwin|wince|emc/
     :windows
   when /darwin|mac os/
     :macosx
   when /linux/
     :linux
   when /solaris|bsd/
     :unix
   else
     raise Error::WebDriverError, "unknown os: #{host_os.inspect}"
   end
 )
end

def windows?
  os == :windows
end

def osx?
  os == :macosx
end

def linux?
  os == :linux
end

BASE = <<-SHELL
# Stack (Haskell) repo
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list

# Elixir (Erlang) repo
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb | dpkg -i

# Clojure (Java)
add-apt-repository ppa:webupd8team/java

# Elm, JS-whatever (Node) repo
###### Forces update
curl -sL https://deb.nodesource.com/setup_6.x | bash -

apt-get upgrade -y
apt-get install -y build-essential libpq-dev git gnupg curl wget

# Install virtualbox additions if not using Hyper-V
#{ "apt-get install -y virtualbox-guest-utils" if !windows? }
SHELL

POSTGRES = <<-SHELL
apt-get install -y postgresql
sudo -u postgres psql -U postgres -d postgres -c "alter user postgres with password 'postgres';"
SHELL

HASKELL = <<-SHELL
apt-get install -y stack
SHELL

ELIXIR = <<-SHELL
apt-get install -y esl-erlang elixir
SHELL

JAVA = <<-SHELL
echo "debconf shared/accepted-oracle-license-v1-1 select true" | sudo debconf-set-selections
echo "debconf shared/accepted-oracle-license-v1-1 seen true" | sudo debconf-set-selections
sudo apt-get install -y oracle-java8-installer
SHELL

CLOJURE = <<-SHELL
#{JAVA}
wget -P ~/bin/ https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod a+x ~/bin/lein
export PATH=$HOME/bin:$PATH
lein
SHELL

RUBY = <<-SHELL
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 &&
\curl -sSL https://get.rvm.io | bash -s stable &&
source ~/.rvm/scripts/rvm &&

echo "rvm_install_on_use_flag=1" >> ~/.rvmrc &&

cd /vagrant
SHELL

NODE = <<-SHELL
sudo apt-get install -y nodejs
sudo npm update -g npm
SHELL

ELM = <<-SHELL
#{NODE}
npm install -g elm
SHELL

Vagrant.configure(2) do |config|
  if windows?
    config.vm.box = "nikel/xerus64"
    config.vm.provider :hyperv do |hyperv|
      hyperv.vmname = "#{File.dirname(__FILE__)}-#{PLATFORMS.last}"
      hyperv.memory = "2048"
    end
  else
    config.vm.box = "ubuntu/xenial64"
    config.vm.provider :virtualbox do |vb|
      vb.gui = false
      # Customize the amount of memory on the VM:
      vb.memory = "2048"
    end
  end

  config.vm.synced_folder ".", "/vagrant", smb_username: ENV['SMB_USERNAME'], smb_password: ENV['SMB_PASSWORD']

  config.vm.provision :shell, inline: BASE

  provs = {
        base: ->(config) { },
    postgres: ->(config) { config.vm.provision :shell, inline: POSTGRES },
     haskell: ->(config) { config.vm.provision :shell, inline: HASKELL },
      elixir: ->(config) { config.vm.provision :shell, inline: ELIXIR },
        java: ->(config) { config.vm.provision :shell, inline: JAVA },
     clojure: ->(config) { config.vm.provision :shell, inline: CLOJURE, privileged: false },
        ruby: ->(config) { config.vm.provision :shell, inline: RUBY, privileged: false },
        node: ->(config) { config.vm.provision :shell, inline: NODE },
         elm: ->(config) { config.vm.provision :shell, inline: ELM }
  }

  PLATFORMS.each { |platform| provs[platform].call(config) }

end
