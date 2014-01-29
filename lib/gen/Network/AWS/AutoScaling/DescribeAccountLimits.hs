{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the limits for the Auto Scaling resources currently allowed for
-- your AWS account. Your AWS account comes with default limits on resources
-- for Auto Scaling. There is a default limit of 20 Auto Scaling groups and
-- 100 launch configurations per region. If you reach the limits for the
-- number of Auto Scaling groups or the launch configurations, you can go to
-- the Support Center and place a request to raise the limits.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAccountLimits &AUTHPARAMS 100 20
-- a32bd184-519d-11e3-a8a4-c1c467cbcc3b.
module Network.AWS.AutoScaling.DescribeAccountLimits where

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

-- | Convenience method utilising default fields where applicable.
describeAccountLimits :: AWS (Either AutoScalingError DescribeAccountLimitsResponse)
describeAccountLimits = undefined DescribeAccountLimits

data DescribeAccountLimits = DescribeAccountLimits
    deriving (Eq, Show, Generic)

instance ToQuery DescribeAccountLimits

instance AWSRequest DescribeAccountLimits where
    type Er DescribeAccountLimits = AutoScalingError
    type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
    request = getQuery service "DescribeAccountLimits"

data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse
    { dalaMaxNumberOfAutoScalingGroups :: Maybe Int
      -- ^ The maximum number of Auto Scaling groups allowed for your AWS account.
    , dalaMaxNumberOfLaunchConfigurations :: Maybe Int
      -- ^ The maximum number of launch configurations allowed for your AWS account.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAccountLimitsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAccountLimitsResponse"
        :| ["DescribeAccountLimitsResult"]
