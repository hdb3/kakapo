.DEFAULT_GOAL := all
.PHONY: kagu core relay vmsetup setup prove_docker
SHELL := /bin/bash
export LIBVIRT_DEFAULT_URI=qemu:///system


all: setup core relay vmsetup kagu

vmimages:
	scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -r 172.16.102.32:vrouter_images .
	mv vrouter_images vmimages

setup:
	testplans/build/ubuntu-dependencies.sh

kagu:
	cd kagu && sg docker ./image_build.sh

vmsetup:
	sg libvirt testplans/build/bootstrap.sh
	sg libvirt "testplans/build/build-nets.sh create"
	sg libvirt "testplans/build/buildjunos.sh jsmoketest"
	sg libvirt "testplans/build/buildvios.sh csmoketest"

core:
	cd core && ./build.sh && ./install.sh

relay:
	cd relay && ./build.sh && ./install.sh

prove_docker: kagu
	for t in bird1 bird2 bird3 relay gobgp hbgp frr bgpd ; do testing/smoketest/runx.sh $$t ; done
