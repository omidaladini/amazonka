SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage
ALIBS := $(wildcard amazonka-*)
LIBS  := $(filter-out amazonka-core, $(ALIBS))

.PHONY: test lint doc

all: install

install: cabal.sandbox.config
	cabal-meta install $(FLAGS)

clean:
	@-rm -rf $(LIBS)

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
