{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.DescribeJobFlows
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | DescribeJobFlows returns a list of job flows that match all of the supplied
-- parameters. The parameters can include a list of job flow IDs, job flow
-- states, and restrictions on job flow creation date and time. Regardless of
-- supplied parameters, only job flows created within the last two months are
-- returned. If no parameters are supplied, then job flows matching either of
-- the following criteria are returned: Job flows created and completed in the
-- last two weeks Job flows created within the last two months that are in one
-- of the following states: RUNNING, WAITING, SHUTTING_DOWN, STARTING Amazon
-- Elastic MapReduce can return a maximum of 512 job flow descriptions. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.DescribeJobFlows Content-Length: 62 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T220330Z
-- X-Amz-Content-Sha256:
-- fce83af973f96f173512aca2845c56862b946feb1de0600326f1365b658a0e39
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- FIXME: Operation documentation for DescribeJobFlows
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=29F98a6f44e05ad54fe1e8b3d1a7101ab08dc3ad348995f89c533693cee2bb3b
-- Accept: */* { "JobFlowIds": ["j-ZKIY4CKQRX72"], "DescriptionType":
-- "EXTENDED" } HTTP/1.1 200 OK x-amzn-RequestId:
-- 634d4142-ed9a-11e2-bbba-b56d7d016ec4 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 1624 Date: Mon, 15 Jul 2013
-- 22:03:31 GMT {"JobFlows": [{ "AmiVersion": "2.3.6", "BootstrapActions": [],
-- "ExecutionStatusDetail": { "CreationDateTime": 1.373923429E9,
-- "EndDateTime": 1.373923995E9, "LastStateChangeReason": "Steps completed",
-- "ReadyDateTime": 1.373923754E9, "StartDateTime": 1.373923754E9, "State":
-- "COMPLETED" }, "Instances": { "HadoopVersion": "1.0.3", "InstanceCount": 1,
-- "InstanceGroups": [{ "CreationDateTime": 1.373923429E9, "EndDateTime":
-- 1.373923995E9, "InstanceGroupId": "ig-3SRUWV3E0NB7K",
-- "InstanceRequestCount": 1, "InstanceRole": "MASTER",
-- "InstanceRunningCount": 0, "InstanceType": "m1.small",
-- "LastStateChangeReason": "Job flow terminated", "Market": "ON_DEMAND",
-- "Name": "Master InstanceGroup", "ReadyDateTime": 1.37392375E9,
-- "StartDateTime": 1.373923646E9, "State": "ENDED" }],
-- "KeepJobFlowAliveWhenNoSteps": false, "MasterInstanceId": "i-8c4fbbef",
-- "MasterInstanceType": "m1.small", "MasterPublicDnsName":
-- "ec2-107-20-46-140.compute-1.amazonaws.com", "NormalizedInstanceHours": 1,
-- "Placement": {"AvailabilityZone": "us-east-1a"}, "TerminationProtected":
-- false }, "JobFlowId": "j-ZKIY4CKQRX72", "Name": "Development Job Flow",
-- "Steps": [{ "ExecutionStatusDetail": { "CreationDateTime": 1.373923429E9,
-- "EndDateTime": 1.373923914E9, "StartDateTime": 1.373923754E9, "State":
-- "COMPLETED" }, "StepConfig": { "ActionOnFailure": "CANCEL_AND_WAIT",
-- "HadoopJarStep": { "Args": [ "-input",
-- "s3://elasticmapreduce/samples/wordcount/input", "-output",
-- "s3://examples-bucket/example-output", "-mapper",
-- "s3://elasticmapreduce/samples/wordcount/wordSplitter.py", "-reducer",
-- "aggregate" ], "Jar":
-- "/home/hadoop/contrib/streaming/hadoop-streaming.jar", "Properties": [] },
-- "Name": "Example Streaming Step" } }], "SupportedProducts": [],
-- "VisibleToAllUsers": false }]}.
module Network.AWS.EMR.DescribeJobFlows where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

-- | Convenience method utilising default fields where applicable.
describeJobFlows :: AWS (Either EMRError DescribeJobFlowsResponse)
describeJobFlows = undefined $ DescribeJobFlows
    { djfiCreatedAfter = Nothing
    , djfiCreatedBefore = Nothing
    , djfiJobFlowIds = []
    , djfiJobFlowStates = []
    }

data DescribeJobFlows = DescribeJobFlows
    { djfiCreatedAfter :: Maybe UTCTime
      -- ^ Return only job flows created after this date and time.
    , djfiCreatedBefore :: Maybe UTCTime
      -- ^ Return only job flows created before this date and time.
    , djfiJobFlowIds :: [Text]
      -- ^ Return only job flows whose job flow ID is contained in this list.
    , djfiJobFlowStates :: [JobFlowExecutionState]
      -- ^ Return only job flows whose state is contained in this list.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeJobFlows

instance AWSRequest DescribeJobFlows where
    type Er DescribeJobFlows = EMRError
    type Rs DescribeJobFlows = DescribeJobFlowsResponse
    request  = getJSON service
    response = responseJSON

data DescribeJobFlowsResponse = DescribeJobFlowsResponse
    { djfirsJobFlows :: [JobFlowDetail]
      -- ^ A list of job flows matching the parameters supplied.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeJobFlowsResponse
