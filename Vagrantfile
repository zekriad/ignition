Base = <<-SHELL
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
apt-get install -y build-essential git gnupg curl wget
apt-get install -y virtualbox-guest-utils
SHELL

Node = <<-SHELL
apt-get install -y nodejs
npm update -g npm
SHELL

Elm = <<-SHELL
npm install -g elm
SHELL


Vagrant.configure(2) do |config|
  config.vm.synced_folder ".", "/vagrant", smb_username: ENV['SMB_USERNAME'], smb_password: ENV['SMB_PASSWORD']

  config.vm.box = "ubuntu/xenial64"
  config.vm.provider :virtualbox do |vb|
    # vb.name   = "#{File.dirname(__FILE__)}-vagrant" # virtualbox copy error on OS X; looks like vagrant calls incorrectly
    vb.memory = "1024"
    vb.gui    = false
  end

  config.vm.provision :shell, inline: Base, privileged: true
  config.vm.provision :shell, inline: Node, privileged: true
  config.vm.provision :shell, inline: Elm, privileged: true
end
