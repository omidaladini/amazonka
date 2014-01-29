{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The SetTopicAttributes action allows a topic owner to set an attribute of
-- the topic to a new value. The following example sets the DisplayName
-- attribute to MyTopicName http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue=MyTopicName
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DisplayName &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- following example sets the delivery policy to 5 total retries
-- http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"http":{"defaultHealthyRetryPolicy":{"numRetries":5}}}
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DeliveryPolicy &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- JSON format for the DeliveryPolicy AttributeValue (linebreaks added for
-- readability): { "http": { "defaultHealthyRetryPolicy": { "minDelayTarget":
-- &lt;int&gt;, "maxDelayTarget": &lt;int&gt;, "numRetries": &lt;int&gt;,
-- "numMaxDelayRetries": &lt;int&gt;, "backoffFunction":
-- "&lt;linear|arithmetic|geometric|exponential&gt;" },
-- "disableSubscriptionOverrides": &lt;boolean&gt;, "defaultThrottlePolicy": {
-- "maxReceivesPerSecond": &lt;int&gt; } }
-- a8763b99-33a7-11df-a9b7-05d48da6f042.
module Network.AWS.SNS.SetTopicAttributes where

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

-- | Convenience method utilising default fields where applicable.
setTopicAttributes :: Text
                   -> Text
                   -> AWS (Either SNSError SetTopicAttributesResponse)
setTopicAttributes p1 p2 = undefined $ SetTopicAttributes
    { staiAttributeName = p1
    , staiTopicArn = p2
    , staiAttributeValue = Nothing
    }

data SetTopicAttributes = SetTopicAttributes
    { staiAttributeName :: !Text
      -- ^ The name of the attribute you want to set. Only a subset of the topic's
      -- attributes are mutable. Valid values: Policy | DisplayName |
      -- DeliveryPolicy.
    , staiAttributeValue :: Maybe Text
      -- ^ The new value for the attribute.
    , staiTopicArn :: !Text
      -- ^ The ARN of the topic to modify.
    } deriving (Eq, Show, Generic)

instance ToQuery SetTopicAttributes

instance AWSRequest SetTopicAttributes where
    type Er SetTopicAttributes = SNSError
    type Rs SetTopicAttributes = SetTopicAttributesResponse
    request = getQuery service "SetTopicAttributes"

data SetTopicAttributesResponse = SetTopicAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML SetTopicAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetTopicAttributesResponse"
        :| ["SetTopicAttributesResult"]
