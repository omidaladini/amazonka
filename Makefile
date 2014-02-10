SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
BIN   := dist/build/generator/generator
DEPS  := vendor/ede vendor/botocore
BOTO  := vendor/botocore/botocore/data/aws

RESTS3 := \
 $(BOTO)/s3.json

RESTXML := \
 $(BOTO)/route53.json \
 $(BOTO)/cloudfront.json

# Issues need to be investigated regarding optional/required values,
# and Textual Booleans, and generally incorrect type info.
RESTJSON := \
 $(BOTO)/elastictranscoder.json

QUERY := \
 $(BOTO)/ses.json \
 $(BOTO)/elasticache.json \
 $(BOTO)/autoscaling.json \
 $(BOTO)/cloudformation.json \
 $(BOTO)/cloudsearch.json \
 $(BOTO)/cloudwatch.json \
 $(BOTO)/elasticbeanstalk.json \
 $(BOTO)/elb.json \
 $(BOTO)/rds.json \
 $(BOTO)/redshift.json \
 $(BOTO)/sns.json \
 $(BOTO)/sqs.json \
 $(BOTO)/sts.json \
 $(BOTO)/ec2.json \
 $(BOTO)/iam.json

JSON := \
 $(BOTO)/dynamodb.json \
 $(BOTO)/kinesis.json \
 $(BOTO)/cloudtrail.json \
 $(BOTO)/datapipeline.json \
 $(BOTO)/directconnect.json \
 $(BOTO)/emr.json \
 $(BOTO)/opsworks.json \
 $(BOTO)/storagegateway.json \
 $(BOTO)/support.json \
 $(BOTO)/swf.json

#MODELS := $(RESTXML) $(RESTS3) $(JSON) $(QUERY)
MODELS := $(BOTO)/ec2.json \
 $(BOTO)/iam.json \
 $(BOTO)/s3.json \
 $(BOTO)/elb.json \
 $(BOTO)/route53.json \
 $(BOTO)/autoscaling.json

.PHONY: test lint doc

all: generator
	rm -rf lib/gen; ./generator $(MODELS) && $(MAKE) install -C lib

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
