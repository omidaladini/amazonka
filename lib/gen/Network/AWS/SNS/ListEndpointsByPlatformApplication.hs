{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListEndpointsByPlatformApplication action lists the endpoints and
-- endpoint attributes for devices in a supported push notification service,
-- such as GCM and APNS. The results for ListEndpointsByPlatformApplication
-- are paginated and return a limited list of endpoints, up to 100. If
-- additional records are available after the first page results, then a
-- NextToken string will be returned. To receive the next page, you call
-- ListEndpointsByPlatformApplication again using the NextToken string
-- received from the previous call. When there are no more records to return,
-- NextToken will be null. For more information, see Using Amazon SNS Mobile
-- Push Notifications. POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &Action=ListEndpointsByPlatformApplication &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Version=2010-03-31
-- &Signature=e6H4sJSCRBBlh%2BaigB%2FtYgp4%2Bjl7dikAQ6WKf%2BMTwNM%3D
-- &Timestamp=2013-07-01T23%3A00%3A52.515Z HTTP/1.1 200 OK ...
-- arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- Enabled true CustomUserData UserId=27576823 Token
-- APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- 9a48768c-dac8-5a60-aec0-3cc27ea08d96.
module Network.AWS.SNS.ListEndpointsByPlatformApplication where

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
listEndpointsByPlatformApplication :: Text
                                   -> AWS (Either SNSError ListEndpointsByPlatformApplicationResponse)
listEndpointsByPlatformApplication p1 = undefined $ ListEndpointsByPlatformApplication
    { lebpaiPlatformApplicationArn = p1
    , lebpaiNextToken = Nothing
    }

data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication
    { lebpaiNextToken :: Maybe Text
      -- ^ NextToken string is used when calling ListEndpointsByPlatformApplication
      -- action to retrieve additional records that are available after the first
      -- page results.
    , lebpaiPlatformApplicationArn :: !Text
      -- ^ PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
    } deriving (Eq, Show, Generic)

instance ToQuery ListEndpointsByPlatformApplication

instance AWSRequest ListEndpointsByPlatformApplication where
    type Er ListEndpointsByPlatformApplication = SNSError
    type Rs ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplicationResponse
    request = getQuery service "ListEndpointsByPlatformApplication"

data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse
    { lebpairsEndpoints :: [Endpoint]
      -- ^ Endpoints returned for ListEndpointsByPlatformApplication action.
    , lebpairsNextToken :: Maybe Text
      -- ^ NextToken string is returned when calling
      -- ListEndpointsByPlatformApplication action if additional records are
      -- available after the first page results.
    } deriving (Eq, Show, Generic)

instance FromXML ListEndpointsByPlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListEndpointsByPlatformApplicationResponse"
        :| ["ListEndpointsByPlatformApplicationResult"]
