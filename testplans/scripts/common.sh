
function stop_host_net {
  sudo ${scriptd}/../nets/nets stop host
}

function start_host_net {
  if sudo ${scriptd}/../nets/nets cycle host
    then
    :
  else
    echo "net start failed"
    exit 1
  fi
}
