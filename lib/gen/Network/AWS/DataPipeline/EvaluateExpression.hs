{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Evaluates a string in the context of a specified object. A task runner can
-- use this action to evaluate SQL queries stored in Amazon S3. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.DescribePipelines Content-Length: 164 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-08785951KAKJEXAMPLE",
-- "objectId": "Schedule", "expression": "Transform started at
-- #{startDateTime} and finished at #{endDateTime}"} x-amzn-RequestId:
-- 02870eb7-0736-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 103 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"evaluatedExpression": "Transform started at
-- 2012-12-12T00:00:00 and finished at 2012-12-21T18:00:00"}.
module Network.AWS.DataPipeline.EvaluateExpression where

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
evaluateExpression :: Text
                   -> Text
                   -> Text
                   -> EvaluateExpression
evaluateExpression p1 p2 p3 = undefined $ EvaluateExpression
    { eeiExpression = p1
    , eeiObjectId = p2
    , eeiPipelineId = p3
    }

data EvaluateExpression = EvaluateExpression
    { eeiExpression :: !Text
      -- ^ The expression to evaluate.
    , eeiObjectId :: !Text
      -- ^ The identifier of the object.
    , eeiPipelineId :: !Text
      -- ^ The identifier of the pipeline.
    } deriving (Eq, Show, Generic)

instance ToJSON EvaluateExpression

instance AWSRequest EvaluateExpression where
    type Er EvaluateExpression = DataPipelineError
    type Rs EvaluateExpression = EvaluateExpressionResponse
    request  = getJSON service
    response = responseJSON

data EvaluateExpressionResponse = EvaluateExpressionResponse
    { eeirsEvaluatedExpression :: !Text
      -- ^ The evaluated expression.
    } deriving (Eq, Show, Generic)

instance FromJSON EvaluateExpressionResponse
