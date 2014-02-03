SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: install

install: cabal.sandbox.config
	cabal-meta install -j $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init --sandbox=../.cabal-sandbox
