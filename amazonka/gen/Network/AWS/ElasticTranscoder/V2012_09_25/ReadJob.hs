{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ReadJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReadJob operation returns detailed information about a job. GET
-- /2012-09-25/jobs/3333333333333-abcde3 HTTP/1.1 Content-Type: charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Job":{ "Id":"3333333333333-abcde3", "Input":{
-- "AspectRatio":"auto", "Container":"mp4", "FrameRate":"auto",
-- "Interlaced":"auto", "Key":"cooking/lasagna.mp4", "Resolution":"auto" },
-- "Output":{ "Key":"", "PresetId":"5555555555555-abcde5", "Rotate":"0",
-- "Status":"Submitted", "StatusDetail":"", "ThumbnailPattern":"{count}" },
-- "PipelineId":"1111111111111-abcde1" } }.
module Network.AWS.ElasticTranscoder.V2012_09_25.ReadJob where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude

data ReadJob = ReadJob
    { _rjrId :: Text
      -- ^ The identifier of the job for which you want to get detailed
      -- information.
    } deriving (Show, Generic)

makeLenses ''ReadJob

instance ToPath ReadJob where
    toPath ReadJob{..} = mconcat
        [ "/2012-09-25/jobs/"
        , toBS _rjrId
        ]

instance ToQuery ReadJob

instance ToHeaders ReadJob

instance ToJSON ReadJob

data ReadJobResponse = ReadJobResponse
    { _rjsJob :: Maybe Job
      -- ^ A section of the response body that provides information about
      -- the job.
    } deriving (Show, Generic)

makeLenses ''ReadJobResponse

instance FromJSON ReadJobResponse

instance AWSRequest ReadJob where
    type Sv ReadJob = ElasticTranscoder
    type Rs ReadJob = ReadJobResponse

    request = get
    response _ = jsonResponse