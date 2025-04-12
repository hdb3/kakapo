.DEFAULT_GOAL := all
SHELL := /bin/bash

all: setup core relay

setup:
	source testplans/build/ubuntu-dependencies.sh
	cd kagu && ./image_build.sh
	touch setup

.PHONY: core
core:
	# $(MAKE) -C core
	cd core && ./build.sh && ./install.sh

.PHONY: relay
relay:
	cd relay && ./build.sh && ./install.sh

