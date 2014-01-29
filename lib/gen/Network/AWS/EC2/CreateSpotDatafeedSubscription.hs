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

-- | Convenience method utilising default fields where applicable.
createSpotDatafeedSubscription :: Text
                               -> AWS (Either EC2Error CreateSpotDatafeedSubscriptionResponse)
createSpotDatafeedSubscription p1 = undefined $ CreateSpotDatafeedSubscription
    { csdsrBucket = p1
    , csdsrDryRun = Nothing
    , csdsrPrefix = Nothing
    }

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { csdsrBucket :: !Text
      -- ^ The Amazon S3 bucket in which to store the Spot Instance datafeed.
    , csdsrDryRun :: Maybe Bool
    , csdsrPrefix :: Maybe Text
      -- ^ The prefix that is prepended to datafeed files.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateSpotDatafeedSubscription

instance AWSRequest CreateSpotDatafeedSubscription where
    type Er CreateSpotDatafeedSubscription = EC2Error
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse
    request = getQuery service "CreateSpotDatafeedSubscription"

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { csdsrrsSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
      -- ^ The SpotDatafeedSubscriptionType data type.
    } deriving (Eq, Show, Generic)

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions
