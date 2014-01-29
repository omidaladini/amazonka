{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreatePlatformApplication action creates a platform application object
-- for one of the supported push notification services, such as APNS and GCM,
-- to which devices and mobile apps may register. You must specify
-- PlatformPrincipal and PlatformCredential attributes when using the
-- CreatePlatformApplication action. The PlatformPrincipal is received from
-- the notification service. For APNS/APNS_SANDBOX, PlatformPrincipal is "SSL
-- certificate". For GCM, PlatformPrincipal is not applicable. For ADM,
-- PlatformPrincipal is "client id". The PlatformCredential is also received
-- from the notification service. For APNS/APNS_SANDBOX, PlatformCredential is
-- "private key". For GCM, PlatformCredential is "API key". For ADM,
-- PlatformCredential is "client secret". The PlatformApplicationArn that is
-- returned when using CreatePlatformApplication is then used as an attribute
-- for the CreatePlatformEndpoint action. For more information, see Using
-- Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.2.key=PlatformPrincipal &SignatureMethod=HmacSHA256
-- &Attributes.entry.1.value=AIzaSyClE2lcV2zEKTLYYo645zfk2jhQPFeyxDo
-- &Attributes.entry.2.value=There+is+no+principal+for+GCM
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Signature=82sHzg1Wfbgisw3i%2BHA2OgBmRktsqUKFinknkq3u%2FQ4%3D
-- &Timestamp=2013-07-01T15%3A49%3A50.354Z &Name=gcmpushapp
-- &Attributes.entry.1.key=PlatformCredential
-- &Action=CreatePlatformApplication &Version=2010-03-31 &SignatureVersion=2
-- &Platform=GCM HTTP/1.1 200 OK ...
-- arn:aws:sns:us-west-2:123456789012:app/GCM/gcmpushapp
-- b6f0e78b-e9d4-5a0e-b973-adc04e8a4ff9.
module Network.AWS.SNS.CreatePlatformApplication where

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
createPlatformApplication :: HashMap Text Text
                          -> Text
                          -> Text
                          -> AWS (Either SNSError CreatePlatformApplicationResponse)
createPlatformApplication p1 p2 p3 = undefined $ CreatePlatformApplication
    { cpaiAttributes = p1
    , cpaiName = p2
    , cpaiPlatform = p3
    }

data CreatePlatformApplication = CreatePlatformApplication
    { cpaiAttributes :: HashMap Text Text
      -- ^ For a list of attributes, see SetPlatformApplicationAttributes.
    , cpaiName :: !Text
      -- ^ Application names must be made up of only uppercase and lowercase ASCII
      -- letters, numbers, underscores, hyphens, and periods, and must be between 1
      -- and 256 characters long.
    , cpaiPlatform :: !Text
      -- ^ The following platforms are supported: ADM (Amazon Device Messaging), APNS
      -- (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google Cloud
      -- Messaging).
    } deriving (Eq, Show, Generic)

instance ToQuery CreatePlatformApplication

instance AWSRequest CreatePlatformApplication where
    type Er CreatePlatformApplication = SNSError
    type Rs CreatePlatformApplication = CreatePlatformApplicationResponse
    request = getQuery service "CreatePlatformApplication"

data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse
    { cpairsPlatformApplicationArn :: Maybe Text
      -- ^ PlatformApplicationArn is returned.
    } deriving (Eq, Show, Generic)

instance FromXML CreatePlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreatePlatformApplicationResponse"
        :| ["CreatePlatformApplicationResult"]
