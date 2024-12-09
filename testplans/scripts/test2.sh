agent1=$(</tmp/agent1.pane)
agent2=$(</tmp/agent2.pane)
agent3=$(</tmp/agent3.pane)
read -p "load good route"

tmux send-keys -t $agent2 "h 7.0.0.6" Enter "p [100,101]" Enter "n [172.16.0.99/32]" Enter "l 500" Enter u Enter

read -p "advertise 666"

tmux send-keys -t $agent3 "h 7.0.0.10" Enter "p [666]" Enter "n [172.16.0.99/32]" Enter "l 500" Enter u Enter

read -p "modify 666 -> 999"

tmux send-keys -t $agent3 "p [999]" Enter u Enter

read -p "reinstate 666"

tmux send-keys -t $agent3 "p [666]" Enter u Enter

read -p "withdraw 666"

tmux send-keys -t $agent3 w Enter

read -p "withdraw [100,101]"

tmux send-keys -t $agent2 w Enter

read -p "all done"

tmux kill-session -t test
