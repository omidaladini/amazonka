{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides more detail about the cluster step.
module Network.AWS.EMR.DescribeStep where

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

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

-- | Convenience method utilising default fields where applicable.
describeStep :: AWS (Either EMRError DescribeStepResponse)
describeStep = undefined $ DescribeStep
    { dsiClusterId = Nothing
    , dsiStepId = Nothing
    }

data DescribeStep = DescribeStep
    { dsiClusterId :: Maybe Text
      -- ^ The identifier of the cluster with steps to describe.
    , dsiStepId :: Maybe Text
      -- ^ The identifier of the step to describe.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeStep

instance AWSRequest DescribeStep where
    type Er DescribeStep = EMRError
    type Rs DescribeStep = DescribeStepResponse
    request  = getJSON service
    response = responseJSON

data DescribeStepResponse = DescribeStepResponse
    { dsirsStep :: Maybe Step
      -- ^ The step details for the requested step identifier.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeStepResponse
