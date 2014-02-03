{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of DKIM tokens for a domain. DKIM tokens are character
-- strings that represent your domain's identity. Using these tokens, you will
-- need to create DNS CNAME records that point to DKIM public keys hosted by
-- Amazon SES. Amazon Web Services will eventually detect that you have
-- updated your DNS records; this detection process may take up to 72 hours.
-- Upon successful detection, Amazon SES will be able to DKIM-sign email
-- originating from that domain. This action is throttled at one request per
-- second. To enable or disable Easy DKIM signing for a domain, use the
-- SetIdentityDkimEnabled action. For more information about creating DNS
-- records using DKIM tokens, go to the Amazon SES Developer Guide. POST /
-- HTTP/1.1 Date: Fri, 29 Jun 2012 22:43:30 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=roXhd+JhEjeBBo5tSERhrptRHSw4XHz6Ra4BXyHIduk=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 136
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyDomainDkim
-- &Domain=example.com &Timestamp=2012-06-29T22%3A43%3A30.000Z
-- &Version=2010-12-01 vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6sf
-- 3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy
-- wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2
-- 9662c15b-c469-11e1-99d1-797d6ecd6414.
module Network.AWS.SES.VerifyDomainDkim where

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
verifyDomainDkim :: Text
                 -> VerifyDomainDkim
verifyDomainDkim p1 = VerifyDomainDkim
    { vddrDomain = p1
    }

data VerifyDomainDkim = VerifyDomainDkim
    { vddrDomain :: !Text
      -- ^ The name of the domain to be verified for Easy DKIM signing.
    } deriving (Eq, Show, Generic)

instance ToQuery VerifyDomainDkim

instance AWSRequest VerifyDomainDkim where
    type Er VerifyDomainDkim = SESError
    type Rs VerifyDomainDkim = VerifyDomainDkimResponse
    request = getQuery service "VerifyDomainDkim"

data VerifyDomainDkimResponse = VerifyDomainDkimResponse
    { vddrrsDkimTokens :: [Text]
      -- ^ A set of character strings that represent the domain's identity. If the
      -- identity is an email address, the tokens represent the domain of that
      -- address. Using these tokens, you will need to create DNS CNAME records that
      -- point to DKIM public keys hosted by Amazon SES. Amazon Web Services will
      -- eventually detect that you have updated your DNS records; this detection
      -- process may take up to 72 hours. Upon successful detection, Amazon SES will
      -- be able to DKIM-sign emails originating from that domain. For more
      -- information about creating DNS records using DKIM tokens, go to the Amazon
      -- SES Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromXML VerifyDomainDkimResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "VerifyDomainDkimResponse"
        :| ["VerifyDomainDkimResult"]
