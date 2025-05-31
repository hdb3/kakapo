#!/bin/bash -e
for bgp in bird1 bird2 bird3 gobgp bgpd frr hbgp relay
  do
    echo "*** $bgp ***"
    testing/smoketest/runx.sh $bgp ||:
    echo "*** DONE *** $bgp ***"
  done

