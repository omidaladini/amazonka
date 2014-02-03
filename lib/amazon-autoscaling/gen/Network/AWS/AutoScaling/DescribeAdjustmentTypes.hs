{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns policy adjustment types for use in the PutScalingPolicy action.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAdjustmentTypes &AUTHPARAMS ChangeInCapacity ExactCapacity
-- PercentChangeInCapacity cc5f0337-b694-11e2-afc0-6544dEXAMPLE.
module Network.AWS.AutoScaling.DescribeAdjustmentTypes where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeAdjustmentTypes :: DescribeAdjustmentTypes
describeAdjustmentTypes = DescribeAdjustmentTypes

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeAdjustmentTypes

instance AWSRequest DescribeAdjustmentTypes where
    type Er DescribeAdjustmentTypes = AutoScalingError
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse
    request = getQuery service "DescribeAdjustmentTypes"

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { dataAdjustmentTypes :: [AdjustmentType]
      -- ^ A list of specific policy adjustment types.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAdjustmentTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAdjustmentTypesResponse"
        :| ["DescribeAdjustmentTypesResult"]
