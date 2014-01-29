{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified activity type. This includes
-- configuration settings provided at registration time as well as other
-- general information about the type. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. Constrain the following parameters by using a Condition element
-- with the appropriate keys. activityType.name: String constraint. The key is
-- swf:activityType.name. activityType.version: String constraint. The key is
-- swf:activityType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. DescribeActivityType Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 03:04:10 GMT X-Amz-Target:
-- SimpleWorkflowService.DescribeActivityType Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=XiGRwOZNLt+ic3VBWvIlRGdcFcRJVSE8J7zyZLU3oXg=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 95 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "activityType": {"name": "activityVerify", "version": "1.0"} }
-- HTTP/1.1 200 OK Content-Length: 387 Content-Type: application/json
-- x-amzn-RequestId: 98d56ff5-3f25-11e1-9b11-7182192d0b57 {"configuration":
-- {"defaultTaskHeartbeatTimeout": "120", "defaultTaskList": {"name":
-- "mainTaskList"}, "defaultTaskScheduleToCloseTimeout": "900",
-- "defaultTaskScheduleToStartTimeout": "300",
-- "defaultTaskStartToCloseTimeout": "600"}, "typeInfo": {"activityType":
-- {"name": "activityVerify", "version": "1.0"}, "creationDate":
-- 1326586446.471, "description": "Verify the customer credit", "status":
-- "REGISTERED"} }.
module Network.AWS.SWF.DescribeActivityType where

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

import Network.AWS.SWF.Service
import Network.AWS.SWF.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeActivityType :: ActivityType
                     -> Text
                     -> DescribeActivityType
describeActivityType p1 p2 = undefined $ DescribeActivityType
    { datiActivityType = p1
    , datiDomain = p2
    }

data DescribeActivityType = DescribeActivityType
    { datiActivityType :: ActivityType
      -- ^ The activity type to describe.
    , datiDomain :: !Text
      -- ^ The name of the domain in which the activity type is registered.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeActivityType

instance AWSRequest DescribeActivityType where
    type Er DescribeActivityType = SWFError
    type Rs DescribeActivityType = DescribeActivityTypeResponse
    request  = getJSON service
    response = responseJSON

data DescribeActivityTypeResponse = DescribeActivityTypeResponse
    { datirsConfiguration :: ActivityTypeConfiguration
      -- ^ The configuration settings registered with the activity type.
    , datirsTypeInfo :: ActivityTypeInfo
      -- ^ General information about the activity type. The status of activity type
      -- (returned in the ActivityTypeInfo structure) can be one of the following.
      -- REGISTERED: The type is registered and available. Workers supporting this
      -- type should be running. DEPRECATED: The type was deprecated using
      -- DeprecateActivityType, but is still in use. You should keep workers
      -- supporting this type running. You cannot create new tasks of this type.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeActivityTypeResponse
