{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetSubscriptionAttribtues action returns all of the properties of a
-- subscription. http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &Action=GetSubscriptionAttributes &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &AWSAccessKeyId=(AWS Access Key Id)
-- &Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D Owner
-- 123456789012 DeliveryPolicy
-- {&quot;healthyRetryPolicy&quot;:{&quot;numRetries&quot;:10}}
-- SubscriptionArn
-- arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- 057f074c-33a7-11df-9540-99d0768312d3.
module Network.AWS.SNS.GetSubscriptionAttributes where

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

import Network.AWS.SNS.Service
import Network.AWS.SNS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getSubscriptionAttributes :: Text
                          -> GetSubscriptionAttributes
getSubscriptionAttributes p1 = GetSubscriptionAttributes
    { gsaiSubscriptionArn = p1
    }

data GetSubscriptionAttributes = GetSubscriptionAttributes
    { gsaiSubscriptionArn :: !Text
      -- ^ The ARN of the subscription whose properties you want to get.
    } deriving (Eq, Show, Generic)

instance ToQuery GetSubscriptionAttributes

instance AWSRequest GetSubscriptionAttributes where
    type Er GetSubscriptionAttributes = SNSError
    type Rs GetSubscriptionAttributes = GetSubscriptionAttributesResponse
    request = getQuery service "GetSubscriptionAttributes"

data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { gsairsAttributes :: HashMap Text Text
      -- ^ A map of the subscription's attributes. Attributes in this map include the
      -- following: SubscriptionArn -- the subscription's ARN TopicArn -- the topic
      -- ARN that the subscription is associated with Owner -- the AWS account ID of
      -- the subscription's owner ConfirmationWasAuthenticated -- true if the
      -- subscription confirmation request was authenticated DeliveryPolicy -- the
      -- JSON serialization of the subscription's delivery policy
      -- EffectiveDeliveryPolicy -- the JSON serialization of the effective delivery
      -- policy that takes into account the topic delivery policy and account system
      -- defaults.
    } deriving (Eq, Show, Generic)

instance FromXML GetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetSubscriptionAttributesResponse"
        :| ["GetSubscriptionAttributesResult"]
