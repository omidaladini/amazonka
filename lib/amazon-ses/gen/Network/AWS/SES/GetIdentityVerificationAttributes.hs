{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of identities (email addresses and/or domains), returns the
-- verification status and (for domain identities) the verification token for
-- each identity. This action is throttled at one request per second. POST /
-- HTTP/1.1 Date: Sat, 12 May 2012 05:27:54 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityVerificationAttributes
-- &Identities.member.1=user%40domain.com &Identities.member.2=domain.com
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z &Version=2010-12-01 domain.com
-- Pending QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0= user@domain.com
-- Pending 1d0c29f1-9bf3-11e1-8ee7-c98a0037a2b6.
module Network.AWS.SES.GetIdentityVerificationAttributes where

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
getIdentityVerificationAttributes :: [Text]
                                  -> GetIdentityVerificationAttributes
getIdentityVerificationAttributes p1 = GetIdentityVerificationAttributes
    { givarIdentities = p1
    }

data GetIdentityVerificationAttributes = GetIdentityVerificationAttributes
    { givarIdentities :: [Text]
      -- ^ A list of identities.
    } deriving (Eq, Show, Generic)

instance ToQuery GetIdentityVerificationAttributes

instance AWSRequest GetIdentityVerificationAttributes where
    type Er GetIdentityVerificationAttributes = SESError
    type Rs GetIdentityVerificationAttributes = GetIdentityVerificationAttributesResponse
    request = getQuery service "GetIdentityVerificationAttributes"

data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    { givarrsVerificationAttributes :: HashMap Text IdentityVerificationAttributes
      -- ^ A map of Identities to IdentityVerificationAttributes objects.
    } deriving (Eq, Show, Generic)

instance FromXML GetIdentityVerificationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetIdentityVerificationAttributesResponse"
        :| ["GetIdentityVerificationAttributesResult"]
