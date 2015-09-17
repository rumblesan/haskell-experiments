BASE_PATH ?= $(PWD)
GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null)

project_name := experiments

## CLI aliases ################################################################
RM := rm -rf
CP := cp
MKDIR := mkdir -p
DOCKER := docker
DOCKER_CP := $(DOCKER) cp
DOCKER_EXEC := $(DOCKER) exec -it
DOCKER_RM := $(DOCKER) rm -vf
DOCKER_RUN := $(DOCKER) run -d
DOCKER_VOLUME := $(DOCKER) run
DOCKER_TASK := $(DOCKER) run --rm -it
###############################################################################

.PHONY: \
	init \
	run \
	stop \
	build \
	update \
	test \
	check \
	connect

image_name := rumblesan/$(project_name)
container_name := $(project_name)

init:
	$(DOCKER) build -t $(image_name) -f Dockerfile .

run:
	$(DOCKER_RUN) \
		-it \
		--name $(container_name) \
		-v $(PWD)/src:/opt/$(project_name)/src \
		-v $(PWD)/$(project_name).cabal:/opt/$(project_name)/$(project_name).cabal \
		$(image_name) \
		sh

stop:
	$(DOCKER_RM) $(container_name)

build:
	@$(DOCKER_EXEC) $(container_name) cabal install --enable-tests --only-dependencies -j
	@$(DOCKER_EXEC) $(container_name) cabal build -j

update:
	@$(DOCKER_EXEC) $(container_name) cabal update

test:
	@$(DOCKER_EXEC) $(container_name) ./dist/build/$(project_name)/$(project_name)

check:
	@$(DOCKER_EXEC) $(container_name) ghc-mod check $(FILENAME)
	@$(DOCKER_EXEC) $(container_name) ghc-mod lint $(FILENAME)

connect:
	$(DOCKER_EXEC) $(container_name) bash

repl:
	$(DOCKER_EXEC) $(container_name) cabal repl

default:
	build

