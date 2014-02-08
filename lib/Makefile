SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: install

install: cabal.sandbox.config
	cabal install $(FLAGS)

clean:
	-rm -rf gen dist
	cabal clean

lint:
	hlint src

doc:
	cabal configure && cabal haddock

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
