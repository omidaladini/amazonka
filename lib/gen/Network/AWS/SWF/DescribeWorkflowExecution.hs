{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified workflow execution including its
-- type and some statistics. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeWorkflowExecution Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:05:18 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=ufQVcSkfUyGPLiS8xbkEBqEc2PmEEE/3Lb9Kr8yozs8=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 127 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "execution": {"workflowId": "20110927-T-1", "runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493"} } HTTP/1.1 200 OK Content-Length:
-- 577 Content-Type: application/json x-amzn-RequestId:
-- 5f85ef79-3f1d-11e1-9e8f-57bb03e21482 {"executionConfiguration":
-- {"childPolicy": "TERMINATE", "executionStartToCloseTimeout": "3600",
-- "taskList": {"name": "specialTaskList"}, "taskStartToCloseTimeout": "600"},
-- "executionInfo": {"cancelRequested": false, "execution": {"runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493", "workflowId": "20110927-T-1"},
-- "executionStatus": "OPEN", "startTimestamp": 1326592619.474, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }, "openCounts":
-- {"openActivityTasks": 0, "openChildWorkflowExecutions": 0,
-- "openDecisionTasks": 1, "openTimers": 0} }.
module Network.AWS.SWF.DescribeWorkflowExecution where

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

-- | Convenience method utilising default fields where applicable.
describeWorkflowExecution :: Text
                          -> WorkflowExecution
                          -> AWS (Either SWFError DescribeWorkflowExecutionResponse)
describeWorkflowExecution p1 p2 = undefined $ DescribeWorkflowExecution
    { dweiDomain = p1
    , dweiExecution = p2
    }

data DescribeWorkflowExecution = DescribeWorkflowExecution
    { dweiDomain :: !Text
      -- ^ The name of the domain containing the workflow execution.
    , dweiExecution :: WorkflowExecution
      -- ^ The workflow execution to describe.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeWorkflowExecution

instance AWSRequest DescribeWorkflowExecution where
    type Er DescribeWorkflowExecution = SWFError
    type Rs DescribeWorkflowExecution = DescribeWorkflowExecutionResponse
    request  = getJSON service
    response = responseJSON

data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse
    { dweirsExecutionConfiguration :: WorkflowExecutionConfiguration
      -- ^ The configuration settings for this workflow execution including timeout
      -- values, tasklist etc.
    , dweirsExecutionInfo :: WorkflowExecutionInfo
      -- ^ Information about the workflow execution.
    , dweirsLatestActivityTaskTimestamp :: Maybe UTCTime
      -- ^ The time when the last activity task was scheduled for this workflow
      -- execution. You can use this information to determine if the workflow has
      -- not made progress for an unusually long period of time and might require a
      -- corrective action.
    , dweirsLatestExecutionContext :: Maybe Text
      -- ^ The latest executionContext provided by the decider for this workflow
      -- execution. A decider can provide an executionContext, which is a free form
      -- string, when closing a decision task using RespondDecisionTaskCompleted.
    , dweirsOpenCounts :: WorkflowExecutionOpenCounts
      -- ^ The number of tasks for this workflow execution. This includes open and
      -- closed tasks of all types.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeWorkflowExecutionResponse
