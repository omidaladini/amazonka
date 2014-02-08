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

import Network.AWS.Core
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
