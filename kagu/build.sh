for image in base bird2 frr gobgp bgpd
  do docker build -t $image $image
done
