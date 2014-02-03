{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.GetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetPlatformApplicationAttributes action retrieves the attributes of the
-- platform application object for the supported push notification services,
-- such as APNS and GCM. For more information, see Using Amazon SNS Mobile
-- Push Notifications. POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &Action=GetPlatformApplicationAttributes &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Version=2010-03-31
-- &Signature=UGMaCq8CCJGSYXO9Ehr2VuHIBYSe6WbxkqgMKRslTK4%3D
-- &Timestamp=2013-07-01T22%3A40%3A50.643Z HTTP/1.1 200 OK ...
-- AllowEndpointPolicies false 74848df2-87f6-55ed-890c-c7be80442462.
module Network.AWS.SNS.GetPlatformApplicationAttributes where

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
getPlatformApplicationAttributes :: Text
                                 -> GetPlatformApplicationAttributes
getPlatformApplicationAttributes p1 = GetPlatformApplicationAttributes
    { gpaaiPlatformApplicationArn = p1
    }

data GetPlatformApplicationAttributes = GetPlatformApplicationAttributes
    { gpaaiPlatformApplicationArn :: !Text
      -- ^ PlatformApplicationArn for GetPlatformApplicationAttributesInput.
    } deriving (Eq, Show, Generic)

instance ToQuery GetPlatformApplicationAttributes

instance AWSRequest GetPlatformApplicationAttributes where
    type Er GetPlatformApplicationAttributes = SNSError
    type Rs GetPlatformApplicationAttributes = GetPlatformApplicationAttributesResponse
    request = getQuery service "GetPlatformApplicationAttributes"

data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse
    { gpaairsAttributes :: HashMap Text Text
      -- ^ Attributes include the following: AllowEndpointPolicies true or false
      -- (default false) - If true, then policies on endpoints will be evaluated to
      -- determine if the topic or user has the rights to send messages to it. If
      -- false, the endpoint policy will be ignored and only the mobile app policy
      -- will be considered. --> EventEndpointCreated -- Topic ARN to which
      -- EndpointCreated event notifications should be sent. EventEndpointDeleted --
      -- Topic ARN to which EndpointDeleted event notifications should be sent.
      -- EventEndpointUpdated -- Topic ARN to which EndpointUpdate event
      -- notifications should be sent. EventDeliveryAttemptFailure Topic ARN to
      -- which DeliveryAttemptFailure event notifications should be sent upon Direct
      -- Publish delivery attempt failures to one of the application's endpoints.
      -- --> EventDeliveryFailure -- Topic ARN to which DeliveryFailure event
      -- notifications should be sent upon Direct Publish delivery failure
      -- (permanent) to one of the application's endpoints. Policy Access policy
      -- controlling who can send messages to endpoints under this mobile app. -->.
    } deriving (Eq, Show, Generic)

instance FromXML GetPlatformApplicationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetPlatformApplicationAttributesResponse"
        :| ["GetPlatformApplicationAttributesResult"]
