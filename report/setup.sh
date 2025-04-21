if ! grep -q venv ~/.profile
then echo 'install venv'
sudo apt install pipx
python3 -m venv ~/.venv
echo -e 'PATH=$HOME/.venv/bin:$PATH' >> ~/.profile
export PATH=$HOME/.venv/bin:$PATH
else echo 'venv already installed'
fi

