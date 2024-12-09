
paneid='-P -F #{pane_id}'

function new_session {
  tmux kill-session -t test || :
  local pane=$( tmux new-session -d $paneid -s test -x 176 -y 48 "echo -e \"\n\nlayout $2\npane $1\" ; sleep 1000" )
  eval "$1=\$pane"
}

function vpane {
  local pane=$( tmux splitw -v -p${3} $paneid -d -t ${1} "echo -e \"\n\npane $2\" ; sleep 1000")
  eval "$2=\$pane"
}

function hpane {
  local pane=$( tmux splitw -h -p${3} $paneid -d -t ${1} "echo -e \"\n\npane $2\" ; sleep 1000")
  eval "$2=\$pane"
}

function layout0 {

# layout 0 - 0a 0a
#            0b 0b
#            0c 0c

  new_session "p0a" "layout0"
  vpane "$p0a" "p0b" 50
  vpane "$p0b" "p0c" 50
}

function layout0a {

# layout 0 - 0a 0a
#            0b 0c

  # p0a=$( tmux new-session -d $paneid -s test -x 176 -y 48 "echo -e \"\n\npane 0a\" ; sleep 1000")
  new_session "p0a" "layout0a"
  vpane "$p0a" "p0b" 50
  hpane "$p0b" "p0c" 50
}

function layout1 {

# layout 1 - 0a 1a
#            0a 1a
#            0b 0b
#            0c 0c

  new_session "p0a" "layout1"
  vpane "$p0a" "p0b" 50
  vpane "$p0b" "p0c" 50
  hpane "$p0a" "p1a" 50
}

function layout2 {

# layout 2 - 0a 1a
#            0b 1b
#            0c 0c

  new_session "p0a" "layout2"
  vpane "$p0a" "p0b" 67
  vpane "$p0b" "p0c" 50
  hpane "$p0a" "p1a" 50
  hpane "$p0b" "p1b" 50
}

function layout3 {

# layout 3 -  0a   1a
#             0b   1b
#            0c 1c 2c

  new_session "p0a" "layout3"
  vpane "$p0a" "p0b" 67
  vpane "$p0b" "p0c" 50
  hpane "$p0a" "p1a" 50
  hpane "$p0b" "p1b" 50
  hpane "$p0c" "p1c" 67
  hpane "$p1c" "p2c" 50
}

# consoles - build horizontal row of equal sized panes
next_console=0

function console {
  if [ $next_console == 0 ]
  then
    tmux kill-session -t console || :
    tmux new-session -d $paneid -s console -x 50 -y 20 $1
  else
    tmux resize-window -t console -R 50
    tmux splitw -h -d -t console $1
    tmux select-layout -t console even-horizontal
  fi
  let next_console+=1
}
