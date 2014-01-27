SHELL  := /usr/bin/env bash
FLAGS  := -j --disable-documentation --disable-library-coverage
BIN    := dist/build/generator/generator
DEPS   := vendor/ede vendor/botocore
BOTO   := vendor/botocore/botocore/data/aws
MODELS := \
 $(BOTO)/route53.json \
 $(BOTO)/ses.json

 # $(BOTO)/s3.json \
 # $(BOTO)/elasticache.json \
 # $(BOTO)/autoscaling.json \
 # $(BOTO)/cloudformation.json \
 # $(BOTO)/cloudsearch.json \
 # $(BOTO)/cloudwatch.json \
 # $(BOTO)/elasticbeanstalk.json \
 # $(BOTO)/elb.json \
 # $(BOTO)/rds.json \
 # $(BOTO)/redshift.json \
 # $(BOTO)/sns.json \
 # $(BOTO)/sqs.json \
 # $(BOTO)/sts.json \
 # $(BOTO)/ec2.json \
 # $(BOTO)/iam.json

.PHONY: test lint doc

all: generator
	rm -rf lib/gen; ./generator $(MODELS) && $(MAKE) -j -C lib

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor/botocore
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

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
