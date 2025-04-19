.DEFAULT_GOAL := all
.PHONY: kagu core relay vmsetup setup
SHELL := /bin/bash

all: setup core relay vmsetup
#all: setup core relay vmsetup kagu

vmimages:
	scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -r 172.16.102.32:vrouter_images .
	mv vrouter_images vmimages

setup:
	testplans/build/ubuntu-dependencies.sh

kagu:
	cd kagu && sg docker ./image_build.sh

vmsetup:
	testplans/build/bootstrap.sh && \
	testplans/build/build-nets.sh create && \
	testplans/build/buildjunos.sh jsmoketest && \
	testplans/build/buildvios.sh csmoketest

core:
	cd core && ./build.sh && ./install.sh

relay:
	cd relay && ./build.sh && ./install.sh
