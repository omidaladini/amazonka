{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets whether all AWS Identity and Access Management (IAM) users under your
-- account can access the specified job flows. This action works on running
-- job flows. You can also set the visibility of a job flow when you launch it
-- using the VisibleToAllUsers parameter of RunJobFlow. The
-- SetVisibleToAllUsers action can be called only by an IAM user who created
-- the job flow or the AWS account that owns the job flow. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.SetVisibleToAllUsers Content-Length: 58 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130715T221616Z
-- X-Amz-Content-Sha256:
-- 2ff32d11eab2383d764ffcb97571454e798689ecd09a7b1bb2327e22b0b930d4
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130715/us-east-1/elasticmapreduce/aws4_request,
-- FIXME: Operation documentation for SetVisibleToAllUsers
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=e1a00b37787d9ccc43c9de32f1f0a73813b0bd6643d4db7762b62a7092d51997
-- Accept: */* { "JobFlowIds": ["j-ZKIY4CKQRX72"], "VisibleToAllUsers": true }
-- HTTP/1.1 200 OK x-amzn-RequestId: 2be9cde9-ed9c-11e2-82b6-2351cde3f33f
-- Content-Type: application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 15
-- Jul 2013 22:16:18 GMT.
module Network.AWS.EMR.SetVisibleToAllUsers where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
setVisibleToAllUsers :: [Text]
                     -> Bool
                     -> SetVisibleToAllUsers
setVisibleToAllUsers p1 p2 = undefined $ SetVisibleToAllUsers
    { svtauiJobFlowIds = p1
    , svtauiVisibleToAllUsers = p2
    }

data SetVisibleToAllUsers = SetVisibleToAllUsers
    { svtauiJobFlowIds :: [Text]
      -- ^ Identifiers of the job flows to receive the new visibility setting.
    , svtauiVisibleToAllUsers :: !Bool
      -- ^ Whether the specified job flows are visible to all IAM users of the AWS
      -- account associated with the job flow. If this value is set to True, all IAM
      -- users of that AWS account can view and, if they have the proper IAM policy
      -- permissions set, manage the job flows. If it is set to False, only the IAM
      -- user that created a job flow can view and manage it.
    } deriving (Eq, Show, Generic)

instance ToJSON SetVisibleToAllUsers

instance AWSRequest SetVisibleToAllUsers where
    type Er SetVisibleToAllUsers = EMRError
    type Rs SetVisibleToAllUsers = SetVisibleToAllUsersResponse
    request  = getJSON service
    response = responseJSON

data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetVisibleToAllUsersResponse
