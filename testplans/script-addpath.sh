tmux set mouse on
tmux set -t test:0 remain-on-exit on
wd="/home/nic/src/hBGP.master/"
testd="/home/nic/src/testplans"
controllerd="/home/nic/src/hBGP.simple-prefix-addpath"
# controllerd="/home/nic/src/hBGP.full-addpath/"
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c $wd          stack run $testd/agent1.conf > /tmp/agent1.pane
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c $wd          stack run $testd/agent2.conf > /tmp/agent2.pane
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c $wd          stack run $testd/agent3.conf > /tmp/agent3.pane
tmux selectl -t test main-horizontal
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c $testd bash test2.sh > /tmp/script.pane
tmux selectl -t test main-horizontal
tmux splitw -P -F "#{pane_id}" -d -t test:0 -c $controllerd stack run $testd/control2.conf > /tmp/control2.pane
tmux killp -t %0
