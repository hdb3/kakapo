# wmctrl -r ':ACTIVE:' -b toggle,fullscreen
# tmux kill-session -t test
#tmux new-session -d -t test
# tmux new-session -t test
read -p "start 7.0.0.1?"
#tmux respawnw -k -t test:0 -c /home/nic/src/hBGP.addpath stack run testplans/7.0.0.1.conf 
tmux splitw -d -t test:0 -c /home/nic/src/hBGP.addpath stack run testplans/7.0.0.1.conf 
read -p "start 7.0.0.2?"
tmux splitw -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/7.0.0.2.conf 
read -p "start 7.0.0.3?"
tmux splitw -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/7.0.0.3.conf 
#tmux selectl -t test:0 tiled

read -p "load initial?"
tmux send-keys -t test:0.1 "h 7.0.0.3" Enter "p [100]" Enter "n [172.16.0.1/32]" Enter u Enter
tmux send-keys -t test:0.2 "h 7.0.0.3" Enter "p [666]" Enter "n [172.16.0.1/32]" Enter "l 300" Enter u Enter

read -p "increase pref?"

tmux send-keys -t test:0.2 "l 50" Enter u Enter

read -p "remove 666"

tmux send-keys -t test:0.2 "p [999]" Enter u Enter

read -p "reinstate 666"

tmux send-keys -t test:0.2 "p [666]" Enter u Enter

read -p "send withdraw?"

tmux send-keys -t test:0.2 w Enter

read -p "all done"
tmux kill-session -t test
