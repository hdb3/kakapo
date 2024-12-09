# sudo apt -y install net-tools whois git tmux openssh-server build-essential libgmp-dev docker.io virt-manager libvirt-clients libffi-dev libncurses-dev zlib1g-dev curl wget tshark wireshark vim-syntastic tcl-expect expect libffi-dev libffi6 libncurses5 libtinfo5
# curl -sSL https://get.haskellstack.org/ | sh
# curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# firefox https://www.google.co.uk/intl/en_uk/chrome/
# firefox https://code.visualstudio.com/download
# sudo apt install ~/Downloads/google-chrome-stable_current_amd64.deb
# sudo apt install ~/Downloads/code_1.44.2-1587059832_amd64.deb
# sudo sed -i -e '/^%sudo/ s/ALL$/NOPASSWD: ALL/' /etc/sudoers
# # TODO - make this idempotent!
# sudo sed -i -e '/^ExecStart/ s/$/ -H tcp:\/\/127.0.0.1:2375/' /usr/lib/systemd/system/docker.service
# sudo systemctl daemon-reload
# sudo systemctl restart docker
# echo "alias docker='DOCKER_HOST=127.0.0.1 docker'" >> ~/.bash_aliases
# alias docker='DOCKER_HOST=127.0.0.1 docker'
# mkdir ~/.local/bin/ || :
# pushd nets
# source install.sh
# popd
mkdir -p ~/test
pushd ~/test
# for url in git@github.com:hdb3/kagu.git git@github.com:hdb3/hBGP.git git@github.com:hdb3/kakapo.git git@bitbucket.org:nrzi/testplans.git git@github.com:hdb3/exa-simple.git
for url in git@github.com:hdb3/kagu.git git@github.com:hdb3/kakapo.git git@github.com:hdb3/exa-simple.git
do
git clone $url
done
popd
