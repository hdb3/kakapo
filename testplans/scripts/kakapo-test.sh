agent1=$(</tmp/agent1.pane)

if [[ $# -ge 1 ]] ; then
  echo "$0"
  echo "kakapo command: $1"
else
  read -p "script error not enough parameters"
  tmux kill-session -t test
fi

sleep 2
tmux send-keys -t $agent1 "$1" Enter

# unconditional exit:

tmux send-keys -t $agent1 'sleep 10 ; tmux kill-session -t test' Enter

while [[ 1 ]] ; do
  read -p "repeat kakapo?"

  if [ "$REPLY" = "q" ] ; then
    tmux kill-window
  fi

  tmux send-keys -t $agent1 "$1" Enter

done
