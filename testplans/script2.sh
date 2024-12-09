tmux set mouse on
# tmux set -t test:0 remain-on-exit on
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent1.conf > /tmp/agent1.pane
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent2.conf > /tmp/agent2.pane
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent3.conf > /tmp/agent3.pane
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c /home/nic/src/hBGP.master/testplans bash test2.sh > /tmp/script.pane
