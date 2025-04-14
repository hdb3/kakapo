.DEFAULT_GOAL := all
SHELL := /bin/bash

all: setup vmsetup core relay

setup:
	source testplans/build/ubuntu-dependencies.sh
	cd kagu && ./image_build.sh
	touch setup

vmsetup:
    testplans/build/bootstrap.sh && \
    testplans/build/build-nets.sh create && \
    testplans/build/buildjunos.sh jsmoketest && \
    testplans/build/buildvios.sh csmoketest && \
 	touch vmsetup

.PHONY: core
core:
	# $(MAKE) -C core
	cd core && ./build.sh && ./install.sh

.PHONY: relay
relay:
	cd relay && ./build.sh && ./install.sh
