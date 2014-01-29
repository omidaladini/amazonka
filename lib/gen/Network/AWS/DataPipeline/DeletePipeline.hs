{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Permanently deletes a pipeline, its pipeline definition and its run
-- history. You cannot query or restore a deleted pipeline. AWS Data Pipeline
-- will attempt to cancel instances associated with the pipeline that are
-- currently being processed by task runners. Deleting a pipeline cannot be
-- undone. To temporarily pause a pipeline instead of deleting it, call
-- SetStatus with the status set to Pause on individual components. Components
-- that are paused by SetStatus can be resumed. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DeletePipeline
-- Content-Length: 50 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- b7a88c81-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
module Network.AWS.DataPipeline.DeletePipeline where

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
deletePipeline :: Text
               -> DeletePipeline
deletePipeline p1 = undefined $ DeletePipeline
    { dpjPipelineId = p1
    }

data DeletePipeline = DeletePipeline
    { dpjPipelineId :: !Text
      -- ^ The identifier of the pipeline to be deleted.
    } deriving (Eq, Show, Generic)

instance ToJSON DeletePipeline

instance AWSRequest DeletePipeline where
    type Er DeletePipeline = DataPipelineError
    type Rs DeletePipeline = DeletePipelineResponse
    request  = getJSON service
    response = responseJSON

data DeletePipelineResponse = DeletePipelineResponse
    deriving (Eq, Show, Generic)

instance FromJSON DeletePipelineResponse
