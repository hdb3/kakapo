#!/bin/bash -e
for bgp in bird bird2 bird3 gobgp bgpd frr hbgp relay
  do
    echo "*** $bgp ***"
    testing/smoketest/run.sh $bgp
    echo "*** DONE *** $bgp ***"
  done

