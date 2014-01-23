SHELL  := /usr/bin/env bash
FLAGS  := -j --disable-documentation --disable-library-coverage
BIN    := dist/build/generator/generator
BOTO   := vendor/botocore/botocore/data/aws
DEPS   := vendor/ede $(BOTO)
MODELS := \
 $(BOTO)/ec2.json
# $(BOTO)/autoscaling.json \
# $(BOTO)/elb.json

.PHONY: test lint doc

all: generator
	rm -rf lib/gen; ./generator $(MODELS) && $(MAKE) -j -C lib

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

generator: build
	cp -f $(BIN) .

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init --sandbox=./.cabal-sandbox

$(BOTO):
	git clone git@github.com:boto/botocore vendor/botocore

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
