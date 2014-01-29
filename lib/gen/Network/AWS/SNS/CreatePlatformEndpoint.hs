{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreatePlatformEndpoint creates an endpoint for a device and mobile app
-- on one of the supported push notification services, such as GCM and APNS.
-- CreatePlatformEndpoint requires the PlatformApplicationArn that is returned
-- from CreatePlatformApplication. The EndpointArn that is returned when using
-- CreatePlatformEndpoint can then be used by the Publish action to send a
-- message to a mobile app or by the Subscribe action for subscription to a
-- topic. For more information, see Using Amazon SNS Mobile Push
-- Notifications. POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &Action=CreatePlatformEndpoint &SignatureMethod=HmacSHA256
-- &CustomUserData=UserId%3D27576823 &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Token=APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- &SignatureVersion=2 &Version=2010-03-31
-- &Signature=Rg5vXBS6OfgPtWkt1u32p1w14uiGh%2BKOicvXNWTEz2w%3D
-- &Timestamp=2013-07-01T15%3A49%3A50.598Z HTTP/1.1 200 OK ...
-- arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- 6613341d-3e15-53f7-bf3c-7e56994ba278.
module Network.AWS.SNS.CreatePlatformEndpoint where

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
createPlatformEndpoint :: Text
                       -> Text
                       -> CreatePlatformEndpoint
createPlatformEndpoint p1 p2 = undefined $ CreatePlatformEndpoint
    { cpeiPlatformApplicationArn = p1
    , cpeiToken = p2
    , cpeiAttributes = Map.empty
    , cpeiCustomUserData = Nothing
    }

data CreatePlatformEndpoint = CreatePlatformEndpoint
    { cpeiAttributes :: HashMap Text Text
      -- ^ For a list of attributes, see SetEndpointAttributes.
    , cpeiCustomUserData :: Maybe Text
      -- ^ Arbitrary user data to associate with the endpoint. SNS does not use this
      -- data. The data must be in UTF-8 format and less than 2KB.
    , cpeiPlatformApplicationArn :: !Text
      -- ^ PlatformApplicationArn returned from CreatePlatformApplication is used to
      -- create a an endpoint.
    , cpeiToken :: !Text
      -- ^ Unique identifier created by the notification service for an app on a
      -- device. The specific name for Token will vary, depending on which
      -- notification service is being used. For example, when using APNS as the
      -- notification service, you need the device token. Alternatively, when using
      -- GCM or ADM, the device token equivalent is called the registration ID.
    } deriving (Eq, Show, Generic)

instance ToQuery CreatePlatformEndpoint

instance AWSRequest CreatePlatformEndpoint where
    type Er CreatePlatformEndpoint = SNSError
    type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse
    request = getQuery service "CreatePlatformEndpoint"

data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse
    { cpeirsEndpointArn :: Maybe Text
      -- ^ EndpointArn returned from CreateEndpoint action.
    } deriving (Eq, Show, Generic)

instance FromXML CreatePlatformEndpointResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreatePlatformEndpointResponse"
        :| ["CreatePlatformEndpointResult"]
