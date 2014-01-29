{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | SetTerminationProtection locks a job flow so the Amazon EC2 instances in
-- the cluster cannot be terminated by user intervention, an API call, or in
-- the event of a job-flow error. The cluster still terminates upon successful
-- completion of the job flow. Calling SetTerminationProtection on a job flow
-- is analogous to calling the Amazon EC2 DisableAPITermination API on all of
-- the EC2 instances in a cluster. SetTerminationProtection is used to prevent
-- accidental termination of a job flow and to ensure that in the event of an
-- error, the instances will persist so you can recover any data stored in
-- their ephemeral instance storage. To terminate a job flow that has been
-- locked by setting SetTerminationProtection to true, you must first unlock
-- the job flow by a subsequent call to SetTerminationProtection in which you
-- set the value to false. For more information, go to Protecting a Job Flow
-- from Termination in the Amazon Elastic MapReduce Developer's Guide. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.SetTerminationProtection Content-Length: 61 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T211420Z
-- X-Amz-Content-Sha256:
-- c362fadae0fce377aa63f04388aeb90c53cedb17a8bfbb8cffcb10c2378137f9
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- FIXME: Operation documentation for SetTerminationProtection
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=764b6aa1a38733cadff35a2e884887e9f1208a422266bc83ac77e8d0b80bd4cf
-- Accept: */* { "JobFlowIds": ["j-3TS0OIYO4NFN"], "TerminationProtected":
-- true } HTTP/1.1 200 OK x-amzn-RequestId:
-- af23b1db-ee5c-11e2-9787-192218ecb460 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 21:14:21 GMT.
module Network.AWS.EMR.SetTerminationProtection where

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
setTerminationProtection :: [Text]
                         -> Bool
                         -> AWS (Either EMRError SetTerminationProtectionResponse)
setTerminationProtection p1 p2 = undefined $ SetTerminationProtection
    { stpiJobFlowIds = p1
    , stpiTerminationProtected = p2
    }

data SetTerminationProtection = SetTerminationProtection
    { stpiJobFlowIds :: [Text]
      -- ^ A list of strings that uniquely identify the job flows to protect. This
      -- identifier is returned by RunJobFlow and can also be obtained from
      -- DescribeJobFlows .
    , stpiTerminationProtected :: !Bool
      -- ^ A Boolean that indicates whether to protect the job flow and prevent the
      -- Amazon EC2 instances in the cluster from shutting down due to API calls,
      -- user intervention, or job-flow error.
    } deriving (Eq, Show, Generic)

instance ToJSON SetTerminationProtection

instance AWSRequest SetTerminationProtection where
    type Er SetTerminationProtection = EMRError
    type Rs SetTerminationProtection = SetTerminationProtectionResponse
    request  = getJSON service
    response = responseJSON

data SetTerminationProtectionResponse = SetTerminationProtectionResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetTerminationProtectionResponse
