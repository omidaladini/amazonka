{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a WorkflowExecutionSignaled event in the workflow execution history
-- and creates a decision task for the workflow execution identified by the
-- given domain, workflowId and runId. The event is recorded with the
-- specified user defined signalName and input (if provided). If a runId is
-- not specified, then the WorkflowExecutionSignaled event is recorded in the
-- history of the current open workflow with the matching workflowId in the
-- domain. If the specified workflow execution is not open, this method fails
-- with UnknownResource. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action. You
-- cannot use an IAM policy to constrain this action's parameters. If the
-- caller does not have sufficient permissions to invoke the action, or the
-- parameter values fall outside the specified constraints, the action fails
-- by throwing OperationNotPermitted. For details and example IAM policies,
-- see Using IAM to Manage Access to Amazon SWF Workflows.
-- SignalWorkflowExecution Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 00:06:18 GMT X-Amz-Target:
-- SimpleWorkflowService.SignalWorkflowExecution Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=lQpBZezK7JNQrXeWuJE+l7S0ZwjOEONCeRyImoyfX+E=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 162 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "signalName": "CancelOrder",
-- "input": "order 3553"} HTTP/1.1 200 OK Content-Length: 0 Content-Type:
-- application/json x-amzn-RequestId: bf78ae15-3f0c-11e1-9914-a356b6ea8bdf.
module Network.AWS.SWF.SignalWorkflowExecution where

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
signalWorkflowExecution :: Text
                        -> Text
                        -> Text
                        -> AWS (Either SWFError SignalWorkflowExecutionResponse)
signalWorkflowExecution p1 p2 p3 = undefined $ SignalWorkflowExecution
    { sweiDomain = p1
    , sweiSignalName = p2
    , sweiWorkflowId = p3
    , sweiInput = Nothing
    , sweiRunId = Nothing
    }

data SignalWorkflowExecution = SignalWorkflowExecution
    { sweiDomain :: !Text
      -- ^ The name of the domain containing the workflow execution to signal.
    , sweiInput :: Maybe Text
      -- ^ Data to attach to the WorkflowExecutionSignaled event in the target
      -- workflow execution's history.
    , sweiRunId :: Maybe Text
      -- ^ The runId of the workflow execution to signal.
    , sweiSignalName :: !Text
      -- ^ The name of the signal. This name must be meaningful to the target
      -- workflow.
    , sweiWorkflowId :: !Text
      -- ^ The workflowId of the workflow execution to signal.
    } deriving (Eq, Show, Generic)

instance ToJSON SignalWorkflowExecution

instance AWSRequest SignalWorkflowExecution where
    type Er SignalWorkflowExecution = SWFError
    type Rs SignalWorkflowExecution = SignalWorkflowExecutionResponse
    request  = getJSON service
    response = responseJSON

data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse
    deriving (Eq, Show, Generic)

instance FromJSON SignalWorkflowExecutionResponse
