{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ActivatePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Validates a pipeline and initiates processing. If the pipeline does not
-- pass validation, activation fails. Call this action to start processing
-- pipeline tasks of a pipeline you've created using the CreatePipeline and
-- PutPipelineDefinition actions. A pipeline cannot be modified after it has
-- been successfully activated. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.ActivatePipeline
-- Content-Length: 39 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} HTTP/1.1 200 x-amzn-RequestId:
-- ee19d5bf-074e-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 2 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
module Network.AWS.DataPipeline.ActivatePipeline where

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
activatePipeline :: Text
                 -> ActivatePipeline
activatePipeline p1 = undefined $ ActivatePipeline
    { apiPipelineId = p1
    }

data ActivatePipeline = ActivatePipeline
    { apiPipelineId :: !Text
      -- ^ The identifier of the pipeline to activate.
    } deriving (Eq, Show, Generic)

instance ToJSON ActivatePipeline

instance AWSRequest ActivatePipeline where
    type Er ActivatePipeline = DataPipelineError
    type Rs ActivatePipeline = ActivatePipelineResponse
    request  = getJSON service
    response = responseJSON

data ActivatePipelineResponse = ActivatePipelineResponse
    deriving (Eq, Show, Generic)

instance FromJSON ActivatePipelineResponse
