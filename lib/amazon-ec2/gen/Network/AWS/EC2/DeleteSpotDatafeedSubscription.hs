{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the data feed for Spot Instances. For conceptual information about
-- Spot Instances, refer to the Amazon Elastic Compute Cloud Developer Guide
-- or Amazon Elastic Compute Cloud User Guide .
module Network.AWS.EC2.DeleteSpotDatafeedSubscription where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription
deleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { dsdssDryRun = Nothing
    }

data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { dsdssDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSpotDatafeedSubscription

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Er DeleteSpotDatafeedSubscription = EC2Error
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse
    request = getQuery service "DeleteSpotDatafeedSubscription"

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteSpotDatafeedSubscriptionResponse
