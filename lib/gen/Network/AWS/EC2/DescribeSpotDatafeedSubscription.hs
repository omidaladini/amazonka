{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the data feed for Spot Instances. For conceptual information
-- about Spot Instances, refer to the Amazon Elastic Compute Cloud Developer
-- Guide or Amazon Elastic Compute Cloud User Guide .
module Network.AWS.EC2.DescribeSpotDatafeedSubscription where

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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { dsdsrDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSpotDatafeedSubscription

instance AWSRequest DescribeSpotDatafeedSubscription where
    type Er DescribeSpotDatafeedSubscription = EC2Error
    type Rs DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionResponse
    request = getQuery service "DescribeSpotDatafeedSubscription"

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { dsdsrrsSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
      -- ^ The Spot Instance datafeed subscription.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions
