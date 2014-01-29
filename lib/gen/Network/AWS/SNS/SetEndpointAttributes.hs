{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The SetEndpointAttributes action sets the attributes for an endpoint for a
-- device on one of the supported push notification services, such as GCM and
-- APNS. For more information, see Using Amazon SNS Mobile Push Notifications.
-- POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=CustomUserData &Action=SetEndpointAttributes
-- &SignatureMethod=HmacSHA256 &Attributes.entry.1.value=My+custom+userdata
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &SignatureVersion=2 &Version=2010-03-31
-- &Signature=CFTGfGOS5vgSU3%2FZgv2h%2FJdWgr2JQdDJSrUU9k38wSM%3D
-- &Timestamp=2013-07-01T22%3A56%3A45.582Z HTTP/1.1 200 OK ...
-- 2fe0bfc7-3e85-5ee5-a9e2-f58b35e85f6a.
module Network.AWS.SNS.SetEndpointAttributes where

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
setEndpointAttributes :: HashMap Text Text
                      -> Text
                      -> AWS (Either SNSError SetEndpointAttributesResponse)
setEndpointAttributes p1 p2 = undefined $ SetEndpointAttributes
    { seaiAttributes = p1
    , seaiEndpointArn = p2
    }

data SetEndpointAttributes = SetEndpointAttributes
    { seaiAttributes :: HashMap Text Text
      -- ^ A map of the endpoint attributes. Attributes in this map include the
      -- following: CustomUserData -- arbitrary user data to associate with the
      -- endpoint. SNS does not use this data. The data must be in UTF-8 format and
      -- less than 2KB. Enabled -- flag that enables/disables delivery to the
      -- endpoint. Message Processor will set this to false when a notification
      -- service indicates to SNS that the endpoint is invalid. Users can set it
      -- back to true, typically after updating Token. Policy access policy
      -- controlling who can send messages to this endpoint . --> Token -- device
      -- token, also referred to as a registration id, for an app and mobile device.
      -- This is returned from the notification service when an app and mobile
      -- device are registered with the notification service.
    , seaiEndpointArn :: !Text
      -- ^ EndpointArn used for SetEndpointAttributes action.
    } deriving (Eq, Show, Generic)

instance ToQuery SetEndpointAttributes

instance AWSRequest SetEndpointAttributes where
    type Er SetEndpointAttributes = SNSError
    type Rs SetEndpointAttributes = SetEndpointAttributesResponse
    request = getQuery service "SetEndpointAttributes"

data SetEndpointAttributesResponse = SetEndpointAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML SetEndpointAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetEndpointAttributesResponse"
        :| ["SetEndpointAttributesResult"]
