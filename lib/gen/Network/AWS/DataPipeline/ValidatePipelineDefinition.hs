{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Tests the pipeline definition with a set of validation checks to ensure
-- that it is well formed and can run without error. Example 1 This example
-- sets an valid pipeline configuration and returns success. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ValidatePipelineDefinition Content-Length: 936 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "pipelineObjects": [ {"id": "Default", "name": "Default", "fields": [
-- {"key": "workerGroup", "stringValue": "MyworkerGroup"} ] }, {"id":
-- "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-09-25T17:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 92c9f347-0776-11e2-8a14-21bb8a1f50ef
-- Content-Type: application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": false} Example 2 This example sets an
-- invalid pipeline configuration and returns the associated set of validation
-- errors. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.ValidatePipelineDefinition Content-Length: 903
-- Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012
-- 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "pipelineObjects": [ {"id": "Default", "name":
-- "Default", "fields": [ {"key": "workerGroup", "stringValue":
-- "MyworkerGroup"} ] }, {"id": "Schedule", "name": "Schedule", "fields": [
-- {"key": "startDateTime", "stringValue": "bad-time"}, {"key": "type",
-- "stringValue": "Schedule"}, {"key": "period", "stringValue": "1 hour"},
-- {"key": "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 496a1f5a-0e6a-11e2-a61c-bd6312c92ddd
-- Content-Type: application/x-amz-json-1.1 Content-Length: 278 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": true, "validationErrors": [ {"errors":
-- ["INVALID_FIELD_VALUE: 'startDateTime' value must be a literal datetime
-- value."], "id": "Schedule"} ] }.
module Network.AWS.DataPipeline.ValidatePipelineDefinition where

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

-- | Convenience method utilising default fields where applicable.
validatePipelineDefinition :: Text
                           -> [PipelineObject]
                           -> AWS (Either DataPipelineError ValidatePipelineDefinitionResponse)
validatePipelineDefinition p1 p2 = undefined $ ValidatePipelineDefinition
    { vpdiPipelineId = p1
    , vpdiPipelineObjects = p2
    }

data ValidatePipelineDefinition = ValidatePipelineDefinition
    { vpdiPipelineId :: !Text
      -- ^ Identifies the pipeline whose definition is to be validated.
    , vpdiPipelineObjects :: [PipelineObject]
      -- ^ A list of objects that define the pipeline changes to validate against the
      -- pipeline.
    } deriving (Eq, Show, Generic)

instance ToJSON ValidatePipelineDefinition

instance AWSRequest ValidatePipelineDefinition where
    type Er ValidatePipelineDefinition = DataPipelineError
    type Rs ValidatePipelineDefinition = ValidatePipelineDefinitionResponse
    request  = getJSON service
    response = responseJSON

data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse
    { vpdirsErrored :: !Bool
      -- ^ If True, there were validation errors.
    , vpdirsValidationErrors :: [ValidationError]
      -- ^ Lists the validation errors that were found by ValidatePipelineDefinition.
    , vpdirsValidationWarnings :: [ValidationWarning]
      -- ^ Lists the validation warnings that were found by
      -- ValidatePipelineDefinition.
    } deriving (Eq, Show, Generic)

instance FromJSON ValidatePipelineDefinitionResponse
