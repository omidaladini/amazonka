{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AddInstanceGroups adds an instance group to a running cluster. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.AddInstanceGroups Content-Length: 168 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T223346Z
-- X-Amz-Content-Sha256:
-- ac5a7193b1283898dd822a4b16ca36963879bb010d2dbe57198439973ab2a7d3
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- FIXME: Operation documentation for AddInstanceGroups
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=4c5e7eb762ea45f292a5cd1a1cc56ed60009e19a9dba3d6e5e4e67e96d43af11
-- Accept: */* { "JobFlowId": "j-3U7TSX5GZFD8Y", "InstanceGroups": [{ "Name":
-- "Task Instance Group", "InstanceRole": "TASK", "InstanceCount": 2,
-- "InstanceType": "m1.small", "Market": "ON_DEMAND" }] } HTTP/1.1 200 OK
-- x-amzn-RequestId: 9da5a349-ed9e-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 71 Date: Mon, 15 Jul 2013
-- 22:33:47 GMT { "InstanceGroupIds": ["ig-294A6A2KWT4WB"], "JobFlowId":
-- "j-3U7TSX5GZFD8Y" }.
module Network.AWS.EMR.AddInstanceGroups where

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
addInstanceGroups :: [InstanceGroupConfig]
                  -> Text
                  -> AWS (Either EMRError AddInstanceGroupsResponse)
addInstanceGroups p1 p2 = undefined $ AddInstanceGroups
    { aigiInstanceGroups = p1
    , aigiJobFlowId = p2
    }

data AddInstanceGroups = AddInstanceGroups
    { aigiInstanceGroups :: [InstanceGroupConfig]
      -- ^ Instance Groups to add.
    , aigiJobFlowId :: !Text
      -- ^ Job flow in which to add the instance groups.
    } deriving (Eq, Show, Generic)

instance ToJSON AddInstanceGroups

instance AWSRequest AddInstanceGroups where
    type Er AddInstanceGroups = EMRError
    type Rs AddInstanceGroups = AddInstanceGroupsResponse
    request  = getJSON service
    response = responseJSON

data AddInstanceGroupsResponse = AddInstanceGroupsResponse
    { aigirsInstanceGroupIds :: [Text]
      -- ^ Instance group IDs of the newly created instance groups.
    , aigirsJobFlowId :: Maybe Text
      -- ^ The job flow ID in which the instance groups are added.
    } deriving (Eq, Show, Generic)

instance FromJSON AddInstanceGroupsResponse
