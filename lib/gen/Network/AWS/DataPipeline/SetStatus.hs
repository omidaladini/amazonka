{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests that the status of an array of physical or logical pipeline
-- objects be updated in the pipeline. This update may not occur immediately,
-- but is eventually consistent. The status that can be set depends on the
-- type of object. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.SetStatus Content-Length: 100 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-0634701J7KEXAMPLE",
-- "objectIds": ["o-08600941GHJWMBR9E2"], "status": "pause"} x-amzn-RequestId:
-- e83b8ab7-076a-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
module Network.AWS.DataPipeline.SetStatus where

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
setStatus :: [Text]
          -> Text
          -> Text
          -> SetStatus
setStatus p1 p2 p3 = undefined $ SetStatus
    { ssiObjectIds = p1
    , ssiPipelineId = p2
    , ssiStatus = p3
    }

data SetStatus = SetStatus
    { ssiObjectIds :: [Text]
      -- ^ Identifies an array of objects. The corresponding objects can be either
      -- physical or components, but not a mix of both types.
    , ssiPipelineId :: !Text
      -- ^ Identifies the pipeline that contains the objects.
    , ssiStatus :: !Text
      -- ^ Specifies the status to be set on all the objects in objectIds. For
      -- components, this can be either PAUSE or RESUME. For instances, this can be
      -- either CANCEL, RERUN, or MARK_FINISHED.
    } deriving (Eq, Show, Generic)

instance ToJSON SetStatus

instance AWSRequest SetStatus where
    type Er SetStatus = DataPipelineError
    type Rs SetStatus = SetStatusResponse
    request  = getJSON service
    response = responseJSON

data SetStatusResponse = SetStatusResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetStatusResponse
