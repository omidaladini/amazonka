{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. The VerifyEmailAddress action is
-- deprecated as of the May 15, 2012 release of Domain Verification. The
-- VerifyEmailIdentity action is now preferred. This action is throttled at
-- one request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:28:27 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=o9NK68jraFg5BnaTQiQhpxj2x1dGONOEFHHgsM6o5as=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 132
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A28%3A27.000Z
-- 8edd7eb2-c864-11e0-9f8f-3da8fc215a7e.
module Network.AWS.SES.VerifyEmailAddress where

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
verifyEmailAddress :: Text
                   -> VerifyEmailAddress
verifyEmailAddress p1 = undefined $ VerifyEmailAddress
    { vearEmailAddress = p1
    }

data VerifyEmailAddress = VerifyEmailAddress
    { vearEmailAddress :: !Text
      -- ^ The email address to be verified.
    } deriving (Eq, Show, Generic)

instance ToQuery VerifyEmailAddress

instance AWSRequest VerifyEmailAddress where
    type Er VerifyEmailAddress = SESError
    type Rs VerifyEmailAddress = VerifyEmailAddressResponse
    request = getQuery service "VerifyEmailAddress"

data VerifyEmailAddressResponse = VerifyEmailAddressResponse
    deriving (Eq, Show, Generic)

instance FromXML VerifyEmailAddressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "VerifyEmailAddressResponse"
        :| ["VerifyEmailAddressResult"]
