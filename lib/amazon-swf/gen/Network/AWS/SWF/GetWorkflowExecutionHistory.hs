{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the history of the specified workflow execution. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call
-- again using the nextPageToken returned by the initial call. This operation
-- is eventually consistent. The results are best effort and may not exactly
-- reflect recent updates and changes. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. You cannot use an IAM policy to constrain this action's parameters.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- GetWorkflowExecutionHistory Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:44:00 GMT X-Amz-Target:
-- SimpleWorkflowService.GetWorkflowExecutionHistory Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=90GENeUWJbEAMWuVI0dcWatHjltMWddXfLjl0MbNOzM=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 175 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "execution": {"workflowId": "20110927-T-1", "runId":
-- "d29e60b5-fa71-4276-a4be-948b0adcd20b"}, "maximumPageSize": 10,
-- "reverseOrder": true} HTTP/1.1 200 OK Content-Length: 2942 Content-Type:
-- application/json x-amzn-RequestId: 5385723f-3ff4-11e1-b118-3bfa5e8e7fc3
-- {"events": [ {"eventId": 11, "eventTimestamp": 1326671603.102, "eventType":
-- "WorkflowExecutionTimedOut", "workflowExecutionTimedOutEventAttributes":
-- {"childPolicy": "TERMINATE", "timeoutType": "START_TO_CLOSE"} },
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 10, "eventTimestamp":
-- 1326670566.124, "eventType": "DecisionTaskScheduled"},
-- {"activityTaskTimedOutEventAttributes": {"latestHeartbeatRecordedEventId":
-- 0, "scheduledEventId": 8, "startedEventId": 0, "timeoutType":
-- "SCHEDULE_TO_START"}, "eventId": 9, "eventTimestamp": 1326670566.124,
-- "eventType": "ActivityTaskTimedOut"},
-- {"activityTaskScheduledEventAttributes": {"activityId": "verification-27",
-- "activityType": {"name": "activityVerify", "version": "1.0"}, "control":
-- "digital music", "decisionTaskCompletedEventId": 7, "heartbeatTimeout":
-- "120", "input": "5634-0056-4367-0923,12/12,437", "scheduleToCloseTimeout":
-- "900", "scheduleToStartTimeout": "300", "startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 8, "eventTimestamp":
-- 1326670266.115, "eventType": "ActivityTaskScheduled"},
-- {"decisionTaskCompletedEventAttributes": {"executionContext": "Black
-- Friday", "scheduledEventId": 5, "startedEventId": 6}, "eventId": 7,
-- "eventTimestamp": 1326670266.103, "eventType": "DecisionTaskCompleted"},
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 5}, "eventId": 6, "eventTimestamp": 1326670161.497,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 5, "eventTimestamp":
-- 1326668752.66, "eventType": "DecisionTaskScheduled"},
-- {"decisionTaskTimedOutEventAttributes": {"scheduledEventId": 2,
-- "startedEventId": 3, "timeoutType": "START_TO_CLOSE"}, "eventId": 4,
-- "eventTimestamp": 1326668752.66, "eventType": "DecisionTaskTimedOut"},
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 2}, "eventId": 3, "eventTimestamp": 1326668152.648,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 2, "eventTimestamp":
-- 1326668003.094, "eventType": "DecisionTaskScheduled"} ], "nextPageToken":
-- "AAAAKgAAAAEAAAAAAAAAATeTvAyvqlQz34ctbGhM5nglWmjzk0hGuHf0g4EO4CblQFku70ukjPgrAHy7Tnp7FaZ0okP8EEWnkfg8gi3Fqy/WVrXyxQaa525D31cIq1owXK21CKR6SQ0Job87G8SHvvqvP7yjLGHlHrRGZUCbJgeEuV4Rp/yW+vKhc8dJ54x7wvpQMwZ+ssG6stTyX26vu1gIDuspk13UrDZa4TbLOFdM0aAocHe3xklKMtD/B4ithem6BWm6CBl/UF7lMfNccwUYEityp1Kht/YrcD9zbJkt1FSt4Y6pgt0njAh4FKRO9nyRyvLmbvgtQXEIQz8hdbjwj3xE1+9ocYwXOCAhVkRsh3OD6F8KHilKfdwg4Xz1jtLXOh4lsCecNb8dS7J9hbRErRbw3rh1Sv415U2Ye23OEfF4Jv7JznpmEyzuq8d2bMyOLjAInQVFK4t1tPo5FAhzdICCXBhRq6Wkt++W9sRQXqqX/HTX5kNomHySZloylPuY5gL5zRj39frInfZk4EXWHwrI+18+erGIHO4nBQpMzO64dMP+A/KtVGCn59rAMmilD6wEE9rH8RuZ03Wkvm9yrJvjrI8/6358n8TMB8OcHoqILkMCAXYiIppnFlm+NWXVqxalHLKOrrNzEZM6qsz3Qj3HV1cpy9P7fnS9QAxrgsAYBoDmdOaFkS3ktAkRa0Sle8STfHi4zKbfIGS7rg=="}.
-- FIXME: Operation documentation for GetWorkflowExecutionHistory
module Network.AWS.SWF.GetWorkflowExecutionHistory where

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
getWorkflowExecutionHistory :: Text
                            -> WorkflowExecution
                            -> GetWorkflowExecutionHistory
getWorkflowExecutionHistory p1 p2 = GetWorkflowExecutionHistory
    { gwehiDomain = p1
    , gwehiExecution = p2
    , gwehiMaximumPageSize = Nothing
    , gwehiNextPageToken = Nothing
    , gwehiReverseOrder = Nothing
    }

data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory
    { gwehiDomain :: !Text
      -- ^ The name of the domain containing the workflow execution.
    , gwehiExecution :: WorkflowExecution
      -- ^ Specifies the workflow execution for which to return the history.
    , gwehiMaximumPageSize :: Maybe Int
      -- ^ Specifies the maximum number of history events returned in one page. The
      -- next page in the result is identified by the NextPageToken returned. By
      -- default 100 history events are returned in a page but the caller can
      -- override this value to a page size smaller than the default. You cannot
      -- specify a page size larger than 100. Note that the number of events may be
      -- less than the maxiumum page size, in which case, the returned page will
      -- have fewer results than the maximumPageSize specified.
    , gwehiNextPageToken :: Maybe Text
      -- ^ If a NextPageToken is returned, the result has more than one pages. To get
      -- the next page, repeat the call and specify the nextPageToken with all other
      -- arguments unchanged.
    , gwehiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the events in reverse order. By default the
      -- results are returned in ascending order of the eventTimeStamp of the
      -- events.
    } deriving (Eq, Show, Generic)

instance ToJSON GetWorkflowExecutionHistory where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GetWorkflowExecutionHistory where
    type Er GetWorkflowExecutionHistory = SWFError
    type Rs GetWorkflowExecutionHistory = GetWorkflowExecutionHistoryResponse
    request  = getJSON service
    response = responseJSON

data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse
    { gwehirsEvents :: [HistoryEvent]
      -- ^ The list of history events.
    , gwehirsNextPageToken :: Maybe Text
      -- ^ The token for the next page. If set, the history consists of more than one
      -- page and the next page can be retrieved by repeating the request with this
      -- token and all other arguments unchanged.
    } deriving (Eq, Show, Generic)

instance FromJSON GetWorkflowExecutionHistoryResponse where
    fromJSON = genericFromJSON jsonOptions

