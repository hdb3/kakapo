agent1="tmux send-keys -t $agent1_pane"
agent2="tmux send-keys -t $agent2_pane"
agent3="tmux send-keys -t $agent3_pane"

read -p "load good route"

$agent2 "h 7.0.0.6" Enter "p [100,101]" Enter "n [172.16.0.99/32]" Enter "l 500" Enter u Enter

read -p "advertise 666"

$agent3 "h 7.0.0.10" Enter "p [666]" Enter "n [172.16.0.99/32]" Enter "l 500" Enter u Enter

read -p "modify 666 -> 999"

$agent3 "p [999]" Enter u Enter

read -p "reinstate 666"

$agent3 "p [666]" Enter u Enter

read -p "withdraw 666"

$agent3 w Enter

read -p "withdraw [100,101]"

$agent2 w Enter

read -p "all done"

tmux wait-for -S test
