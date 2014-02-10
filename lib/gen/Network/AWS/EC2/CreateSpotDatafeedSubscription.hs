{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates the data feed for Spot Instances, enabling you to view Spot
-- Instance usage logs. You can create one data feed per account. For
-- conceptual information about Spot Instances, refer to the Amazon Elastic
-- Compute Cloud Developer Guide or Amazon Elastic Compute Cloud User Guide .
module Network.AWS.EC2.CreateSpotDatafeedSubscription where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createSpotDatafeedSubscription :: Text
                               -- ^ The Amazon S3 bucket in which to store the Spot Instance datafeed.
                               -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription p1 = CreateSpotDatafeedSubscription
    { csdsBucket = p1
    , csdsDryRun = Nothing
    , csdsPrefix = Nothing
    }

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { csdsBucket :: !Text
      -- ^ The Amazon S3 bucket in which to store the Spot Instance datafeed.
    , csdsDryRun :: Maybe Bool
    , csdsPrefix :: Maybe Text
      -- ^ The prefix that is prepended to datafeed files.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateSpotDatafeedSubscription

instance AWSRequest CreateSpotDatafeedSubscription where
    type Er CreateSpotDatafeedSubscription = EC2Error
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse
    request  = postQuery service "CreateSpotDatafeedSubscription"
    response = responseXML

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { csdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
      -- ^ The SpotDatafeedSubscriptionType data type.
    } deriving (Eq, Show, Generic)

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions
