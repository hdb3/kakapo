.DEFAULT_GOAL := all
.PHONY: kagu core relay
SHELL := /bin/bash

all: setup kagu vmsetup core relay

setup:
	testplans/build/ubuntu-dependencies.sh && \
	touch setup

kagu:
	cd kagu && ./image_build.sh

vmsetup:
	testplans/build/bootstrap.sh && \
	testplans/build/build-nets.sh create && \
	testplans/build/buildjunos.sh jsmoketest && \
	testplans/build/buildvios.sh csmoketest && \
	touch vmsetup

core:
	# $(MAKE) -C core
	cd core && ./build.sh && ./install.sh

relay:
	cd relay && ./build.sh && ./install.sh
