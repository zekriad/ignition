# -*- mode: ruby -*-
# vi: set ft=ruby :

PLATFORMS = %i(clojure) # %i(postgres haskell elixir clojure ruby elm)

BASE = <<-SHELL
# Stack (Haskell) repo
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu xenial main'|tee /etc/apt/sources.list.d/fpco.list

# Elixir (Erlang) repo
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb | dpkg -i

# Clojure (Java)
add-apt-repository ppa:webupd8team/java

# Elm, JS-whatever (Node) repo
###### Forces update
curl -sL https://deb.nodesource.com/setup_6.x | bash -

apt-get upgrade -y
apt-get install -y build-essential libpq-dev git gnupg curl wget
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

CLOJURE = <<-SHELL
echo debconf shared/accepted-oracle-license-v1-1 select true | \
sudo debconf-set-selections
echo debconf shared/accepted-oracle-license-v1-1 seen true | \
sudo debconf-set-selections
sudo apt-get install -y oracle-java8-installer

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

ELM = <<-SHELL
apt-get install -y nodejs
npm update -g npm
npm install -g elm
SHELL

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "nikel/xerus64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"
  config.vm.synced_folder ".", "/vagrant", smb_username: ENV['SMB_USERNAME'], smb_password: ENV['SMB_PASSWORD']

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider :hyperv do |provider|
    # Customize the amount of memory on the VM:
    provider.vmname = "#{File.dirname(__FILE__)}-#{PLATFORMS.last}"
    provider.memory = "2048"
  end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision :shell, path: "bootstrap.sh"
  config.vm.provision :shell, inline: BASE

  # provs = {}
  provs = {
    postgres: ->(config) { config.vm.provision :shell, inline: POSTGRES },
     haskell: ->(config) { config.vm.provision :shell, inline: HASKELL },
      elixir: ->(config) { config.vm.provision :shell, inline: ELIXIR },
     clojure: ->(config) { config.vm.provision :shell, inline: CLOJURE, privileged: false },
        ruby: ->(config) { config.vm.provision :shell, inline: RUBY, privileged: false },
         elm: ->(config) { config.vm.provision :shell, inline: ELM }
  }

  PLATFORMS.each { |platform| provs[platform].call(config) }

end
