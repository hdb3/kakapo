.DEFAULT_GOAL := all
.PHONY: kagu core relay
SHELL := /bin/bash

all: groups setup kagu vmsetup core relay

groups:
	for group in libvirt docker ; do sudo groupadd $group ; sudo usermod -a -G $group $USERNAME ; done
	touch groups

setup:
	testplans/build/ubuntu-dependencies.sh && \
	touch setup

kagu:
	cd kagu && sg docker ./image_build.sh

vmsetup:
	testplans/build/bootstrap.sh && \
	testplans/build/build-nets.sh create && \
	testplans/build/buildjunos.sh jsmoketest && \
	testplans/build/buildvios.sh csmoketest && \
	touch vmsetup

core:
	cd core && ./build.sh && ./install.sh

relay:
	cd relay && ./build.sh && ./install.sh
