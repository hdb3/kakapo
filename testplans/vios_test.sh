virsh start base
fping -q -r 100 192.168.120.202
if time hbgp_functional_test/run.vm.sh
  then virsh destroy base
else
  if time hbgp_functional_test/run.vm.sh
    then virsh destroy base
  fi
fi

