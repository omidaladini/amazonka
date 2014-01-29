{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetEndpointAttributes retrieves the endpoint attributes for a device on
-- one of the supported push notification services, such as GCM and APNS. For
-- more information, see Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Action=GetEndpointAttributes &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &SignatureVersion=2 &Version=2010-03-31
-- &Signature=%2B2egbEoT4npw3p5H3wiIdzZBoTn4KI3UWmMFyBsHH9c%3D
-- &Timestamp=2013-07-01T22%3A44%3A56.515Z HTTP/1.1 200 OK ... Enabled true
-- CustomUserData UserId=01234567 Token
-- APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- 6c725a19-a142-5b77-94f9-1055a9ea04e7.
module Network.AWS.SNS.GetEndpointAttributes where

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
getEndpointAttributes :: Text
                      -> AWS (Either SNSError GetEndpointAttributesResponse)
getEndpointAttributes p1 = undefined $ GetEndpointAttributes
    { geaiEndpointArn = p1
    }

data GetEndpointAttributes = GetEndpointAttributes
    { geaiEndpointArn :: !Text
      -- ^ EndpointArn for GetEndpointAttributes input.
    } deriving (Eq, Show, Generic)

instance ToQuery GetEndpointAttributes

instance AWSRequest GetEndpointAttributes where
    type Er GetEndpointAttributes = SNSError
    type Rs GetEndpointAttributes = GetEndpointAttributesResponse
    request = getQuery service "GetEndpointAttributes"

data GetEndpointAttributesResponse = GetEndpointAttributesResponse
    { geairsAttributes :: HashMap Text Text
      -- ^ Attributes include the following: CustomUserData -- arbitrary user data to
      -- associate with the endpoint. SNS does not use this data. The data must be
      -- in UTF-8 format and less than 2KB. Enabled -- flag that enables/disables
      -- delivery to the endpoint. Message Processor will set this to false when a
      -- notification service indicates to SNS that the endpoint is invalid. Users
      -- can set it back to true, typically after updating Token. Policy access
      -- policy controlling who can send messages to this endpoint . --> Token --
      -- device token, also referred to as a registration id, for an app and mobile
      -- device. This is returned from the notification service when an app and
      -- mobile device are registered with the notification service.
    } deriving (Eq, Show, Generic)

instance FromXML GetEndpointAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetEndpointAttributesResponse"
        :| ["GetEndpointAttributesResult"]
