{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified workflow type. This includes
-- configuration settings specified when the type was registered and other
-- information such as creation date, current status, etc. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. Constrain the following parameters by
-- using a Condition element with the appropriate keys. workflowType.name:
-- String constraint. The key is swf:workflowType.name. workflowType.version:
-- String constraint. The key is swf:workflowType.version. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeWorkflowType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 22:40:40 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=iGt8t83OmrURqu0pKYbcW6mNdjXbFomevCBPUPQEbaM=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 102 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } HTTP/1.1 200 OK Content-Length: 348 Content-Type: application/json
-- x-amzn-RequestId: f35a8e7f-3fc9-11e1-a23a-99d60383ae71 {"configuration":
-- {"defaultChildPolicy": "TERMINATE", "defaultExecutionStartToCloseTimeout":
-- "3600", "defaultTaskList": {"name": "mainTaskList"},
-- "defaultTaskStartToCloseTimeout": "600"}, "typeInfo": {"creationDate":
-- 1326481174.027, "description": "Handle customer orders", "status":
-- "REGISTERED", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } }.
module Network.AWS.SWF.DescribeWorkflowType where

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
describeWorkflowType :: Text
                     -> WorkflowType
                     -> DescribeWorkflowType
describeWorkflowType p1 p2 = DescribeWorkflowType
    { dwtiDomain = p1
    , dwtiWorkflowType = p2
    }

data DescribeWorkflowType = DescribeWorkflowType
    { dwtiDomain :: !Text
      -- ^ The name of the domain in which this workflow type is registered.
    , dwtiWorkflowType :: WorkflowType
      -- ^ The workflow type to describe.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeWorkflowType where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeWorkflowType where
    type Er DescribeWorkflowType = SWFError
    type Rs DescribeWorkflowType = DescribeWorkflowTypeResponse
    request  = getJSON service
    response = responseJSON

data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse
    { dwtirsConfiguration :: WorkflowTypeConfiguration
      -- ^ Configuration settings of the workflow type registered through
      -- RegisterWorkflowType.
    , dwtirsTypeInfo :: WorkflowTypeInfo
      -- ^ General information about the workflow type. The status of the workflow
      -- type (returned in the WorkflowTypeInfo structure) can be one of the
      -- following. REGISTERED: The type is registered and available. Workers
      -- supporting this type should be running. DEPRECATED: The type was deprecated
      -- using DeprecateWorkflowType, but is still in use. You should keep workers
      -- supporting this type running. You cannot create new workflow executions of
      -- this type.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeWorkflowTypeResponse where
    fromJSON = genericFromJSON jsonOptions

