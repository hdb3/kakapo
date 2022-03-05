gcc -g -O3 -pthread -D_GNU_SOURCE relayapp.c librelay.c util.c -o relayapp
sudo chown root:root relayapp ; sudo chmod u+s relayapp
