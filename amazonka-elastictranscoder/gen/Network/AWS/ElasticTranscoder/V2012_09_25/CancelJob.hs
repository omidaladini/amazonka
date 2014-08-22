{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.CancelJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CancelJob operation cancels an unfinished job. You can only cancel a
-- job that has a status of Submitted. To prevent a pipeline from starting to
-- process a job while you're getting the job identifier, use
-- UpdatePipelineStatus to temporarily pause the pipeline. DELETE
-- /2012-09-25/jobs/3333333333333-abcde3 HTTP/1.1 Content-Type: charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Success":"true" }.
module Network.AWS.ElasticTranscoder.V2012_09_25.CancelJob where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data CancelJob = CancelJob
    { _cjvId :: Text
      -- ^ The identifier of the job that you want to cancel. To get a list
      -- of the jobs (including their jobId) that have a status of
      -- Submitted, use the ListJobsByStatus API action.
    } deriving (Show, Generic)

makeLenses ''CancelJob

instance ToPath CancelJob where
    toPath CancelJob{..} = mconcat
        [ "/2012-09-25/jobs/"
        , toBS _cjvId
        ]

instance ToQuery CancelJob

instance ToHeaders CancelJob

instance ToJSON CancelJob

data CancelJobResponse = CancelJobResponse
    deriving (Eq, Show, Generic)

makeLenses ''CancelJobResponse

instance AWSRequest CancelJob where
    type Sv CancelJob = ElasticTranscoder
    type Rs CancelJob = CancelJobResponse

    request = delete
    response _ = nullaryResponse CancelJobResponse