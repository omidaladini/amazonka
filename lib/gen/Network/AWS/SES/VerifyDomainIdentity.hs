{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies a domain. This action is throttled at one request per second. POST
-- / HTTP/1.1 Date: Sat, 12 May 2012 05:24:02 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Wr+6RCfV+QgjLki2dtIrlecMK9+RrsDaTG5uWneDAu8=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 139
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyDomainIdentity
-- &Domain=domain.com &Timestamp=2012-05-12T05%3A24%3A02.000Z
-- &Version=2010-12-01 QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0=
-- 94f6368e-9bf2-11e1-8ee7-c98a0037a2b6.
module Network.AWS.SES.VerifyDomainIdentity where

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

data VerifyDomainIdentity = VerifyDomainIdentity
    { vdirDomain :: !Text
      -- ^ The domain to be verified.
    } deriving (Eq, Show, Generic)

instance ToQuery VerifyDomainIdentity

instance AWSRequest VerifyDomainIdentity where
    type Er VerifyDomainIdentity = SESError
    type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse
    request = getQuery service "VerifyDomainIdentity"

data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    { vdirrsVerificationToken :: !Text
      -- ^ A TXT record that must be placed in the DNS settings for the domain, in
      -- order to complete domain verification.
    } deriving (Eq, Show, Generic)

instance FromXML VerifyDomainIdentityResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "VerifyDomainIdentityResponse"
        :| ["VerifyDomainIdentityResult"]
