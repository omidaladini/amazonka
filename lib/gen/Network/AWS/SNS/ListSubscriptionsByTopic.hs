{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListSubscriptionsByTopic action returns a list of the subscriptions to
-- a specific topic. Each call returns a limited list of subscriptions, up to
-- 100. If there are more subscriptions, a NextToken is also returned. Use the
-- NextToken parameter in a new ListSubscriptionsByTopic call to get further
-- results. http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &Action=ListSubscriptionsByTopic &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- arn:aws:sns:us-east-1:123456789012:My-Topic email
-- arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- 123456789012 example@amazon.com b9275252-3774-11df-9540-99d0768312d3.
module Network.AWS.SNS.ListSubscriptionsByTopic where

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
listSubscriptionsByTopic :: Text
                         -> AWS (Either SNSError ListSubscriptionsByTopicResponse)
listSubscriptionsByTopic p1 = undefined $ ListSubscriptionsByTopic
    { lsbtiTopicArn = p1
    , lsbtiNextToken = Nothing
    }

data ListSubscriptionsByTopic = ListSubscriptionsByTopic
    { lsbtiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListSubscriptionsByTopic request.
    , lsbtiTopicArn :: !Text
      -- ^ The ARN of the topic for which you wish to find subscriptions.
    } deriving (Eq, Show, Generic)

instance ToQuery ListSubscriptionsByTopic

instance AWSRequest ListSubscriptionsByTopic where
    type Er ListSubscriptionsByTopic = SNSError
    type Rs ListSubscriptionsByTopic = ListSubscriptionsByTopicResponse
    request = getQuery service "ListSubscriptionsByTopic"

instance AWSPager ListSubscriptionsByTopic where
    next rq rs
        | Just x <- lsbtirsNextToken rs = Just $ rq { lsbtiNextToken = Just x }
        | otherwise = Nothing

data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    { lsbtirsNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListSubscriptionsByTopic request. This
      -- element is returned if there are more subscriptions to retrieve.
    , lsbtirsSubscriptions :: [Subscription]
      -- ^ A list of subscriptions.
    } deriving (Eq, Show, Generic)

instance FromXML ListSubscriptionsByTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListSubscriptionsByTopicResponse"
        :| ["ListSubscriptionsByTopicResult"]
