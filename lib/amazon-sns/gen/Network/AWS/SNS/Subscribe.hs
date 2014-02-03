{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Subscribe action prepares to subscribe an endpoint by sending the
-- endpoint a confirmation message. To actually create a subscription, the
-- endpoint owner must call the ConfirmSubscription action with the token from
-- the confirmation message. Confirmation tokens are valid for three days.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &Endpoint=example%40amazon.com &Protocol=email &Action=Subscribe
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=1%2FeGaDphxXq%2Fa89x6HvKh%2Fc1yLGXzuhS7vS2MslToDM%3D pending
-- confirmation a169c740-3766-11df-8963-01868b7c937a.
module Network.AWS.SNS.Subscribe where

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
subscribe :: Text
          -> Text
          -> Subscribe
subscribe p1 p2 = Subscribe
    { siProtocol = p1
    , siTopicArn = p2
    , siEndpoint = Nothing
    }

data Subscribe = Subscribe
    { siEndpoint :: Maybe Text
      -- ^ The endpoint that you want to receive notifications. Endpoints vary by
      -- protocol: For the http protocol, the endpoint is an URL beginning with
      -- "http://" For the https protocol, the endpoint is a URL beginning with
      -- "https://" For the email protocol, the endpoint is an email address For the
      -- email-json protocol, the endpoint is an email address For the sms protocol,
      -- the endpoint is a phone number of an SMS-enabled device For the sqs
      -- protocol, the endpoint is the ARN of an Amazon SQS queue For the
      -- application protocol, the endpoint is the EndpointArn of a mobile app and
      -- device.
    , siProtocol :: !Text
      -- ^ The protocol you want to use. Supported protocols include: http -- delivery
      -- of JSON-encoded message via HTTP POST https -- delivery of JSON-encoded
      -- message via HTTPS POST email -- delivery of message via SMTP email-json --
      -- delivery of JSON-encoded message via SMTP sms -- delivery of message via
      -- SMS sqs -- delivery of JSON-encoded message to an Amazon SQS queue
      -- application -- delivery of JSON-encoded message to an EndpointArn for a
      -- mobile app and device.
    , siTopicArn :: !Text
      -- ^ The ARN of the topic you want to subscribe to.
    } deriving (Eq, Show, Generic)

instance ToQuery Subscribe

instance AWSRequest Subscribe where
    type Er Subscribe = SNSError
    type Rs Subscribe = SubscribeResponse
    request = getQuery service "Subscribe"

data SubscribeResponse = SubscribeResponse
    { sirsSubscriptionArn :: Maybe Text
      -- ^ The ARN of the subscription, if the service was able to create a
      -- subscription immediately (without requiring endpoint owner confirmation).
    } deriving (Eq, Show, Generic)

instance FromXML SubscribeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SubscribeResponse"
        :| ["SubscribeResult"]
