{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.PutNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to this topic can have messages for events
-- delivered to an endpoint such as a web server or email address. For more
-- information see Get Email Notifications When Your Auto Scaling Group
-- Changes A new PutNotificationConfiguration overwrites an existing
-- configuration.
module Network.AWS.AutoScaling.PutNotificationConfiguration where

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
putNotificationConfiguration :: ResourceName
                             -> [Text]
                             -> ResourceName
                             -> AWS (Either AutoScalingError PutNotificationConfigurationResponse)
putNotificationConfiguration p1 p2 p3 = undefined $ PutNotificationConfiguration
    { pnctAutoScalingGroupName = p1
    , pnctNotificationTypes = p2
    , pnctTopicARN = p3
    }

data PutNotificationConfiguration = PutNotificationConfiguration
    { pnctAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group.
    , pnctNotificationTypes :: [Text]
      -- ^ The type of event that will cause the notification to be sent. For details
      -- about notification types supported by Auto Scaling, see
      -- DescribeAutoScalingNotificationTypes.
    , pnctTopicARN :: !ResourceName
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic.
    } deriving (Eq, Show, Generic)

instance ToQuery PutNotificationConfiguration

instance AWSRequest PutNotificationConfiguration where
    type Er PutNotificationConfiguration = AutoScalingError
    type Rs PutNotificationConfiguration = PutNotificationConfigurationResponse
    request = getQuery service "PutNotificationConfiguration"

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML PutNotificationConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PutNotificationConfigurationResponse"
        :| ["PutNotificationConfigurationResult"]
