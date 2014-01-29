{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current status of Easy DKIM signing for an entity. For domain
-- name identities, this action also returns the DKIM tokens that are required
-- for Easy DKIM signing, and whether Amazon SES has successfully verified
-- that these tokens have been published. This action takes a list of
-- identities as input and returns the following information for each: Whether
-- Easy DKIM signing is enabled or disabled. A set of DKIM tokens that
-- represent the identity. If the identity is an email address, the tokens
-- represent the domain of that address. Whether Amazon SES has successfully
-- verified the DKIM tokens published in the domain's DNS. This information is
-- only returned for domain name identities, not for email addresses. This
-- action is throttled at one request per second. For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide. POST / HTTP/1.1 Date: Fri, 29 Jun 2012 22:41:32 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=MJdhrIAt3c4BRC6jdzueMM+AJLEx17bnIHjZwlSenyk=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 165
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetIdentityDkimAttributes
-- &Identities.member.1=example.com &Timestamp=2012-06-29T22%3A41%3A32.000Z
-- &Version=2010-12-01 amazon.com true Success
-- vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f
-- 3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy
-- wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2
-- bb5a105d-c468-11e1-82eb-dff885ccc06a.
module Network.AWS.SES.GetIdentityDkimAttributes where

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

-- | Convenience method utilising default fields where applicable.
getIdentityDkimAttributes :: [Text]
                          -> AWS (Either SESError GetIdentityDkimAttributesResponse)
getIdentityDkimAttributes p1 = undefined $ GetIdentityDkimAttributes
    { gidarIdentities = p1
    }

data GetIdentityDkimAttributes = GetIdentityDkimAttributes
    { gidarIdentities :: [Text]
      -- ^ A list of one or more verified identities - email addresses, domains, or
      -- both.
    } deriving (Eq, Show, Generic)

instance ToQuery GetIdentityDkimAttributes

instance AWSRequest GetIdentityDkimAttributes where
    type Er GetIdentityDkimAttributes = SESError
    type Rs GetIdentityDkimAttributes = GetIdentityDkimAttributesResponse
    request = getQuery service "GetIdentityDkimAttributes"

data GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse
    { gidarrsDkimAttributes :: HashMap Text IdentityDkimAttributes
      -- ^ The DKIM attributes for an email address or a domain.
    } deriving (Eq, Show, Generic)

instance FromXML GetIdentityDkimAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetIdentityDkimAttributesResponse"
        :| ["GetIdentityDkimAttributesResult"]
