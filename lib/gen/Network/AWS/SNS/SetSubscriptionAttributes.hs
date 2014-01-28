{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The SetSubscriptionAttributes action allows a subscription owner to set an
-- attribute of the topic to a new value. The following example sets the
-- delivery policy to 5 total retries http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"healthyRetryPolicy":{"numRetries":5}}
-- &SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &AttributeName=DeliveryPolicy &Action=SetSubscriptionAttributes
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key Id)
-- &Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The JSON format
-- for the DeliveryPolicy AttributeValue (linebreaks added for readability): {
-- "healthyRetryPolicy": { "minDelayTarget": , "maxDelayTarget": ,
-- "numRetries": , "numMaxDelayRetries": , "backoffFunction": "" },
-- "throttlePolicy": { "maxReceivesPerSecond": } }
-- a8763b99-33a7-11df-a9b7-05d48da6f042.
module Network.AWS.SNS.SetSubscriptionAttributes where

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

data SetSubscriptionAttributes = SetSubscriptionAttributes
    { ssaiAttributeName :: !Text
      -- ^ The name of the attribute you want to set. Only a subset of the
      -- subscriptions attributes are mutable. Valid values: DeliveryPolicy.
    , ssaiAttributeValue :: Maybe Text
      -- ^ The new value for the attribute in JSON format.
    , ssaiSubscriptionArn :: !Text
      -- ^ The ARN of the subscription to modify.
    } deriving (Eq, Show, Generic)

instance ToQuery SetSubscriptionAttributes

instance AWSRequest SetSubscriptionAttributes where
    type Er SetSubscriptionAttributes = SNSError
    type Rs SetSubscriptionAttributes = SetSubscriptionAttributesResponse
    request = getQuery service "SetSubscriptionAttributes"

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML SetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetSubscriptionAttributesResponse"
        :| ["SetSubscriptionAttributesResult"]
