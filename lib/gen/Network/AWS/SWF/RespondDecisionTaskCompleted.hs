{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.RespondDecisionTaskCompleted
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by deciders to tell the service that the DecisionTask identified by
-- the taskToken has successfully completed. The decisions argument specifies
-- the list of decisions made while processing the task. A
-- DecisionTaskCompleted event is added to the workflow history. The
-- executionContext specified is attached to the event in the workflow
-- execution history. Access Control If an IAM policy grants permission to use
-- RespondDecisionTaskCompleted, it can express permissions for the list of
-- decisions in the decisions parameter. Each of the decisions has one or more
-- parameters, much like a regular API call. To allow for policies to be as
-- readable as possible, you can express permissions on decisions as if they
-- were actual API calls, including applying conditions to some parameters.
-- For more information, see Using IAM to Manage Access to Amazon SWF
-- Workflows. RespondDecisionTaskCompleted Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 23:31:06 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondDecisionTaskCompleted Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=FL4ouCb8n6j5egcKOXoa+5Vctc8WmA91B2ekKnks2J8=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 1184 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAQLPoqDSLcx4ksNCEQZCyEBqpKhE+FgFSOvHd9zlCROacKYHh640MkANx2y9YM3CQnec0kEb1oRvB6DxKesTY3U/UQhvBqPY7E4BYE6hkDj/NmSbt9EwEJ/a+WD+oc2sDNfeVz2x+6wjb5vQdFKwBoQ6MDWLFbAhcgK+ymoRjoBHrPsrNLX3IA6sQaPmQRZQs3FRZonoVzP6uXMCZPnCZQULFjU1kTM8VHzH7ywqWKVmmdvnqyREOCT9VqmYbhLntJXsDj+scAvuNy17MCX9M9AJ7V/5qrLCeYdWA4FBQgY4Ew6IC+dge/UZdVMmpW/uB7nvSk6owQIhapPh5pEUwwY/yNnoVLTiPOz9KzZlANyw7uDchBRLvUJORFtpP9ZQIouNP8QOvFWm7Idc50ahwGEdTCiG+KDXV8kAzx7wKHs7l1TXYkC15x0h3XPH0MdLeEjipv98EpZaMIVtgGSdRjluOjNWEL2zowZByitleI5bdvxZdgalAXXKEnbYE6/rfLGReAJKdh2n0dmTMI+tK7uuxIWX6F4ocqSI1Xb2x5zZ",
-- "decisions": [ {"decisionType": "ScheduleActivityTask",
-- "scheduleActivityTaskDecisionAttributes": {"activityType": {"name":
-- "activityVerify", "version": "1.0"}, "activityId": "verification-27",
-- "control": "digital music", "input": "5634-0056-4367-0923,12/12,437",
-- "scheduleToCloseTimeout": "900", "taskList": {"name": "specialTaskList"},
-- "scheduleToStartTimeout": "300", "startToCloseTimeout": "600",
-- "heartbeatTimeout": "120"} } ], "executionContext": "Black Friday"}
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: feef79b5-3fd0-11e1-9a27-0760db01a4a8.
module Network.AWS.SWF.RespondDecisionTaskCompleted where

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
respondDecisionTaskCompleted :: Text
                             -> RespondDecisionTaskCompleted
respondDecisionTaskCompleted p1 = undefined $ RespondDecisionTaskCompleted
    { rdtciTaskToken = p1
    , rdtciDecisions = []
    , rdtciExecutionContext = Nothing
    }

data RespondDecisionTaskCompleted = RespondDecisionTaskCompleted
    { rdtciDecisions :: [Decision]
      -- ^ The list of decisions (possibly empty) made by the decider while processing
      -- this decision task. See the docs for the Decision structure for details.
    , rdtciExecutionContext :: Maybe Text
      -- ^ User defined context to add to workflow execution.
    , rdtciTaskToken :: !Text
      -- ^ The taskToken from the DecisionTask. The taskToken is generated by the
      -- service and should be treated as an opaque value. If the task is passed to
      -- another process, its taskToken must also be passed. This enables it to
      -- provide its progress and respond with results.
    } deriving (Eq, Show, Generic)

instance ToJSON RespondDecisionTaskCompleted

instance AWSRequest RespondDecisionTaskCompleted where
    type Er RespondDecisionTaskCompleted = SWFError
    type Rs RespondDecisionTaskCompleted = RespondDecisionTaskCompletedResponse
    request  = getJSON service
    response = responseJSON

data RespondDecisionTaskCompletedResponse = RespondDecisionTaskCompletedResponse
    deriving (Eq, Show, Generic)

instance FromJSON RespondDecisionTaskCompletedResponse
