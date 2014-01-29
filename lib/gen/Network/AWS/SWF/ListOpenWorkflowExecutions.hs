{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of open workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple pages.
-- To retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. Constrain the
-- following parameters by using a Condition element with the appropriate
-- keys. tagFilter.tag: String constraint. The key is swf:tagFilter.tag.
-- typeFilter.name: String constraint. The key is swf:typeFilter.name.
-- typeFilter.version: String constraint. The key is swf:typeFilter.version.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- ListOpenWorkflowExecutions POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 23:51:04 GMT X-Amz-Target:
-- SimpleWorkflowService.ListOpenWorkflowExecutions Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=4kUhpZUp37PgpeOKHlWTsZi+Pq3Egw4mTkPNiEUgp28=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 151 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "startTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "music purchase"} } HTTP/1.1 200 OK
-- Content-Length: 313 Content-Type: application/json x-amzn-RequestId:
-- 9efeff4b-3f0a-11e1-9e8f-57bb03e21482 {"executionInfos": [
-- {"cancelRequested": false, "execution": {"runId":
-- "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "workflowId": "20110927-T-1"},
-- "executionStatus": "OPEN", "startTimestamp": 1326585031.619, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
module Network.AWS.SWF.ListOpenWorkflowExecutions where

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
listOpenWorkflowExecutions :: Text
                           -> ExecutionTimeFilter
                           -> AWS (Either SWFError ListOpenWorkflowExecutionsResponse)
listOpenWorkflowExecutions p1 p2 = undefined $ ListOpenWorkflowExecutions
    { loweiDomain = p1
    , loweiStartTimeFilter = p2
    , loweiExecutionFilter = Nothing
    , loweiMaximumPageSize = Nothing
    , loweiNextPageToken = Nothing
    , loweiReverseOrder = Nothing
    , loweiTagFilter = Nothing
    , loweiTypeFilter = Nothing
    }

data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions
    { loweiDomain :: !Text
      -- ^ The name of the domain that contains the workflow executions to list.
    , loweiExecutionFilter :: Maybe WorkflowExecutionFilter
      -- ^ If specified, only workflow executions matching the workflow id specified
      -- in the filter are returned. executionFilter, typeFilter and tagFilter are
      -- mutually exclusive. You can specify at most one of these in a request.
    , loweiMaximumPageSize :: Maybe Int
      -- ^ The maximum number of results returned in each page. The default is 100,
      -- but the caller can override this value to a page size smaller than the
      -- default. You cannot specify a page size greater than 100. Note that the
      -- number of executions may be less than the maxiumum page size, in which
      -- case, the returned page will have fewer results than the maximumPageSize
      -- specified.
    , loweiNextPageToken :: Maybe Text
      -- ^ If on a previous call to this method a NextPageToken was returned, the
      -- results are being paginated. To get the next page of results, repeat the
      -- call with the returned token and all other arguments unchanged.
    , loweiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the results in reverse order. By default the
      -- results are returned in descending order of the start time of the
      -- executions.
    , loweiStartTimeFilter :: ExecutionTimeFilter
      -- ^ Workflow executions are included in the returned results based on whether
      -- their start times are within the range specified by this filter.
    , loweiTagFilter :: Maybe TagFilter
      -- ^ If specified, only executions that have the matching tag are listed.
      -- executionFilter, typeFilter and tagFilter are mutually exclusive. You can
      -- specify at most one of these in a request.
    , loweiTypeFilter :: Maybe WorkflowTypeFilter
      -- ^ If specified, only executions of the type specified in the filter are
      -- returned. executionFilter, typeFilter and tagFilter are mutually exclusive.
      -- You can specify at most one of these in a request.
    } deriving (Eq, Show, Generic)

instance ToJSON ListOpenWorkflowExecutions

instance AWSRequest ListOpenWorkflowExecutions where
    type Er ListOpenWorkflowExecutions = SWFError
    type Rs ListOpenWorkflowExecutions = ListOpenWorkflowExecutionsResponse
    request  = getJSON service
    response = responseJSON

data ListOpenWorkflowExecutionsResponse = ListOpenWorkflowExecutionsResponse
    { loweirsExecutionInfos :: [WorkflowExecutionInfo]
      -- ^ The list of workflow information structures.
    , loweirsNextPageToken :: Maybe Text
      -- ^ The token of the next page in the result. If set, the results have more
      -- than one page. The next page can be retrieved by repeating the request with
      -- this token and all other arguments unchanged.
    } deriving (Eq, Show, Generic)

instance FromJSON ListOpenWorkflowExecutionsResponse
