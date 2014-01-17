SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
BIN   := dist/build/generator/generator
DEPS  := vendor/ede vendor/botocore

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS))) && cp -f $(BIN) .

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
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init --sandbox=./.cabal-sandbox

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
