gcc -g -O3 -pthread -D_GNU_SOURCE relayapp.c librelay.c util.c -o relayapp
sudo chown root:root relayapp ; sudo chmod u+s relayapp

gcc -g -O3 -pthread -D_GNU_SOURCE sinkapp.c libsink.c util.c -o sinkapp
sudo chown root:root sinkapp ; sudo chmod u+s sinkapp
