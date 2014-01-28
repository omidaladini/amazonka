{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The SetPlatformApplicationAttributes action sets the attributes of the
-- platform application object for the supported push notification services,
-- such as APNS and GCM. For more information, see Using Amazon SNS Mobile
-- Push Notifications. POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=EventEndpointCreated&PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &Action=SetPlatformApplicationAttributes &SignatureMethod=HmacSHA256
-- &Attributes.entry.1.value=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Atopicarn
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Version=2010-03-31
-- &Signature=06L2TsW3jiH%2FGKDYuT8w4NojSrTf4Ig2GKqGeJPhPT4%3D
-- &Timestamp=2013-07-01T22%3A53%3A17.800Z HTTP/1.1 200 OK ...
-- cf577bcc-b3dc-5463-88f1-3180b9412395.
module Network.AWS.SNS.SetPlatformApplicationAttributes where

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

data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes
    { spaaiAttributes :: HashMap Text Text
      -- ^ A map of the platform application attributes. Attributes in this map
      -- include the following: PlatformCredential -- The credential received from
      -- the notification service. For APNS/APNS_SANDBOX, PlatformCredential is
      -- "private key". For GCM, PlatformCredential is "API key". For ADM,
      -- PlatformCredential is "client secret". PlatformPrincipal -- The principal
      -- received from the notification service. For APNS/APNS_SANDBOX,
      -- PlatformPrincipal is "SSL certificate". For GCM, PlatformPrincipal is not
      -- applicable. For ADM, PlatformPrincipal is "client id".
      -- AllowEndpointPolicies true or false (default false) - If true, then
      -- policies on endpoints will be evaluated to determine if the topic or user
      -- has the rights to send messages to it. If false, the endpoint policy will
      -- be ignored and only the mobile app policy will be considered. -->
      -- EventEndpointCreated -- Topic ARN to which EndpointCreated event
      -- notifications should be sent. EventEndpointDeleted -- Topic ARN to which
      -- EndpointDeleted event notifications should be sent. EventEndpointUpdated --
      -- Topic ARN to which EndpointUpdate event notifications should be sent.
      -- EventDeliveryAttemptFailure Topic ARN to which DeliveryAttemptFailure event
      -- notifications should be sent upon Direct Publish delivery attempt failures
      -- to one of the application's endpoints. --> EventDeliveryFailure -- Topic
      -- ARN to which DeliveryFailure event notifications should be sent upon Direct
      -- Publish delivery failure (permanent) to one of the application's endpoints.
      -- Policy Access policy controlling who can send messages to endpoints under
      -- this mobile app. -->.
    , spaaiPlatformApplicationArn :: !Text
      -- ^ PlatformApplicationArn for SetPlatformApplicationAttributes action.
    } deriving (Eq, Show, Generic)

instance ToQuery SetPlatformApplicationAttributes

instance AWSRequest SetPlatformApplicationAttributes where
    type Er SetPlatformApplicationAttributes = SNSError
    type Rs SetPlatformApplicationAttributes = SetPlatformApplicationAttributesResponse
    request = getQuery service "SetPlatformApplicationAttributes"

data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML SetPlatformApplicationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetPlatformApplicationAttributesResponse"
        :| ["SetPlatformApplicationAttributesResult"]
