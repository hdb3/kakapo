gcc -g -O3 -pthread -D_GNU_SOURCE main.c util.c -o relay
gcc -g -O3 -pthread -D_GNU_SOURCE relay2.c util.c -o relay2
sudo chown root:root relay ; sudo chmod u+s relay
sudo chown root:root relay2 ; sudo chmod u+s relay2
