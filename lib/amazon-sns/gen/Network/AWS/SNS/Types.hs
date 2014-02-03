{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SNS.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.SNS.Service

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a
-- topic's attributes, use GetTopicAttributes.
newtype Topic = Topic
    { tTopicArn :: Maybe Text
      -- ^ The topic's ARN.
    } deriving (Eq, Show, Generic)

instance ToQuery Topic

instance FromXML Topic where
    fromXMLOptions = xmlOptions

instance ToXML Topic where
    toXMLOptions = xmlOptions

-- | A wrapper type for the attributes of an SNS subscription.
data Subscription = Subscription
    { sEndpoint :: Maybe Text
      -- ^ The subscription's endpoint (format depends on the protocol).
    , sOwner :: Maybe Text
      -- ^ The subscription's owner.
    , sProtocol :: Maybe Text
      -- ^ The subscription's protocol.
    , sSubscriptionArn :: Maybe Text
      -- ^ The subscription's ARN.
    , sTopicArn :: Maybe Text
      -- ^ The ARN of the subscription's topic.
    } deriving (Eq, Show, Generic)

instance ToQuery Subscription

instance FromXML Subscription where
    fromXMLOptions = xmlOptions

instance ToXML Subscription where
    toXMLOptions = xmlOptions

-- | Platform application object.
data PlatformApplication = PlatformApplication
    { paAttributes :: HashMap Text Text
      -- ^ Attributes for platform application object.
    , paPlatformApplicationArn :: Maybe Text
      -- ^ PlatformApplicationArn for platform application object.
    } deriving (Eq, Show, Generic)

instance ToQuery PlatformApplication

instance FromXML PlatformApplication where
    fromXMLOptions = xmlOptions

instance ToXML PlatformApplication where
    toXMLOptions = xmlOptions

-- | Endpoint for mobile app and device.
data Endpoint = Endpoint
    { eAttributes :: HashMap Text Text
      -- ^ Attributes for endpoint.
    , eEndpointArn :: Maybe Text
      -- ^ EndpointArn for mobile app and device.
    } deriving (Eq, Show, Generic)

instance ToQuery Endpoint

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions

instance ToXML Endpoint where
    toXMLOptions = xmlOptions
