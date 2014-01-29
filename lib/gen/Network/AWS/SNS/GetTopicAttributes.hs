{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetTopicAttributes action returns all of the properties of a topic.
-- Topic properties returned might differ based on the authorization of the
-- user. http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &Action=GetTopicAttributes &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key Id)
-- &Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D Owner
-- 123456789012 Policy {
-- &quot;Version&quot;:&quot;2008-10-17&quot;,&quot;Id&quot;:&quot;us-east-1/698519295917/test__default_policy_ID&quot;,&quot;Statement&quot;
-- :
-- [{&quot;Effect&quot;:&quot;Allow&quot;,&quot;Sid&quot;:&quot;us-east-1/698519295917/test__default_statement_ID&quot;,&quot;Principal&quot;
-- : {&quot;AWS&quot;:
-- &quot;*&quot;},&quot;Action&quot;:[&quot;SNS:GetTopicAttributes&quot;,&quot;SNS:SetTopicAttributes&quot;,&quot;SNS:AddPermission&quot;,&quot;SNS:RemovePermission&quot;,&quot;SNS:DeleteTopic&quot;,&quot;SNS:Subscribe&quot;,&quot;SNS:ListSubscriptionsByTopic&quot;,&quot;SNS:Publish&quot;,&quot;SNS:Receive&quot;],&quot;Resource&quot;:&quot;arn:aws:sns:us-east-1:698519295917:test&quot;,&quot;Condition&quot;
-- : {&quot;StringLike&quot; : {&quot;AWS:SourceArn&quot;:
-- &quot;arn:aws:*:*:698519295917:*&quot;}}}]} TopicArn
-- arn:aws:sns:us-east-1:123456789012:My-Topic
-- 057f074c-33a7-11df-9540-99d0768312d3.
module Network.AWS.SNS.GetTopicAttributes where

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
getTopicAttributes :: Text
                   -> GetTopicAttributes
getTopicAttributes p1 = undefined $ GetTopicAttributes
    { gtaiTopicArn = p1
    }

data GetTopicAttributes = GetTopicAttributes
    { gtaiTopicArn :: !Text
      -- ^ The ARN of the topic whose properties you want to get.
    } deriving (Eq, Show, Generic)

instance ToQuery GetTopicAttributes

instance AWSRequest GetTopicAttributes where
    type Er GetTopicAttributes = SNSError
    type Rs GetTopicAttributes = GetTopicAttributesResponse
    request = getQuery service "GetTopicAttributes"

data GetTopicAttributesResponse = GetTopicAttributesResponse
    { gtairsAttributes :: HashMap Text Text
      -- ^ A map of the topic's attributes. Attributes in this map include the
      -- following: TopicArn -- the topic's ARN Owner -- the AWS account ID of the
      -- topic's owner Policy -- the JSON serialization of the topic's access
      -- control policy DisplayName -- the human-readable name used in the "From"
      -- field for notifications to email and email-json endpoints
      -- SubscriptionsPending -- the number of subscriptions pending confirmation on
      -- this topic SubscriptionsConfirmed -- the number of confirmed subscriptions
      -- on this topic SubscriptionsDeleted -- the number of deleted subscriptions
      -- on this topic DeliveryPolicy -- the JSON serialization of the topic's
      -- delivery policy EffectiveDeliveryPolicy -- the JSON serialization of the
      -- effective delivery policy that takes into account system defaults.
    } deriving (Eq, Show, Generic)

instance FromXML GetTopicAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetTopicAttributesResponse"
        :| ["GetTopicAttributesResult"]
