{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Task runners call ReportTaskRunnerHeartbeat every 15 minutes to indicate
-- that they are operational. In the case of AWS Data Pipeline Task Runner
-- launched on a resource managed by AWS Data Pipeline, the web service can
-- use this call to detect when the task runner application has failed and
-- restart a new instance. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ReportTaskRunnerHeartbeat Content-Length: 84 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"taskrunnerId": "1234567890", "workerGroup":
-- "wg-12345", "hostname": "example.com"} Status: x-amzn-RequestId:
-- b3104dc5-0734-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 20 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"terminate": false}.
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat where

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

import Network.AWS.DataPipeline.Service
import Network.AWS.DataPipeline.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
reportTaskRunnerHeartbeat :: Text
                          -> ReportTaskRunnerHeartbeat
reportTaskRunnerHeartbeat p1 = ReportTaskRunnerHeartbeat
    { rtrhiTaskrunnerId = p1
    , rtrhiHostname = Nothing
    , rtrhiWorkerGroup = Nothing
    }

data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat
    { rtrhiHostname :: Maybe Text
      -- ^ The public DNS name of the calling task runner.
    , rtrhiTaskrunnerId :: !Text
      -- ^ The identifier of the task runner. This value should be unique across your
      -- AWS account. In the case of AWS Data Pipeline Task Runner launched on a
      -- resource managed by AWS Data Pipeline, the web service provides a unique
      -- identifier when it launches the application. If you have written a custom
      -- task runner, you should assign a unique identifier for the task runner.
    , rtrhiWorkerGroup :: Maybe Text
      -- ^ Indicates the type of task the task runner is configured to accept and
      -- process. The worker group is set as a field on objects in the pipeline when
      -- they are created. You can only specify a single value for workerGroup in
      -- the call to ReportTaskRunnerHeartbeat. There are no wildcard values
      -- permitted in workerGroup, the string must be an exact, case-sensitive,
      -- match.
    } deriving (Eq, Show, Generic)

instance ToJSON ReportTaskRunnerHeartbeat where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ReportTaskRunnerHeartbeat where
    type Er ReportTaskRunnerHeartbeat = DataPipelineError
    type Rs ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeatResponse
    request  = getJSON service
    response = responseJSON

data ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse
    { rtrhirsTerminate :: !Bool
      -- ^ Indicates whether the calling task runner should terminate. If True, the
      -- task runner that called ReportTaskRunnerHeartbeat should terminate.
    } deriving (Eq, Show, Generic)

instance FromJSON ReportTaskRunnerHeartbeatResponse where
    fromJSON = genericFromJSON jsonOptions

