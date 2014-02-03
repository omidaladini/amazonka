{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of verified identities (email addresses and/or domains),
-- returns a structure describing identity notification attributes. This
-- action is throttled at one request per second. For more information about
-- feedback notification, see the Amazon SES Developer Guide. POST / HTTP/1.1
-- Date: Fri, 15 Jun 2012 20:51:42 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=ee9aH6tUW5wBPoh01Tz3w4H+z4avrMmvmRYbfORC7OI=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 173
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityNotificationAttributes
-- &Identities.member.1=user%40example.com
-- &Timestamp=2012-06-15T20%3A51%3A42.000Z &Version=2010-12-01
-- user@example.com true arn:aws:sns:us-east-1:123456789012:example
-- arn:aws:sns:us-east-1:123456789012:example
-- e038e509-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.GetIdentityNotificationAttributes where

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

import Network.AWS.SES.Service
import Network.AWS.SES.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getIdentityNotificationAttributes :: [Text]
                                  -> GetIdentityNotificationAttributes
getIdentityNotificationAttributes p1 = GetIdentityNotificationAttributes
    { ginarIdentities = p1
    }

data GetIdentityNotificationAttributes = GetIdentityNotificationAttributes
    { ginarIdentities :: [Text]
      -- ^ A list of one or more identities.
    } deriving (Eq, Show, Generic)

instance ToQuery GetIdentityNotificationAttributes

instance AWSRequest GetIdentityNotificationAttributes where
    type Er GetIdentityNotificationAttributes = SESError
    type Rs GetIdentityNotificationAttributes = GetIdentityNotificationAttributesResponse
    request = getQuery service "GetIdentityNotificationAttributes"

data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    { ginarrsNotificationAttributes :: HashMap Text IdentityNotificationAttributes
      -- ^ A map of Identity to IdentityNotificationAttributes.
    } deriving (Eq, Show, Generic)

instance FromXML GetIdentityNotificationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetIdentityNotificationAttributesResponse"
        :| ["GetIdentityNotificationAttributesResult"]
