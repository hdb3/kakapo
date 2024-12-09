#!/bin/bash -e

if [[ -z $1 ]] ; then script=active-script ; else script="$1" ; fi
if [[ -f "$script" ]] ; then echo "using $script"; else echo "$script is not a regular file"; exit 1 ; fi

if tmux kill-session -t test ; then echo "killed old session" ; else echo "no session to kill" ; fi
tmux new-session -d -s test -x 400 -y 400 
tmux set -t test mouse on
gnome-terminal --full-screen -- tmux attach -t test:0 
tmux send -t test:0.0 "bash -xe $script" Enter
tmux wait-for test
