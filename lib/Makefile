SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

add-sources: cabal.sandbox.config $(DEPS)

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
