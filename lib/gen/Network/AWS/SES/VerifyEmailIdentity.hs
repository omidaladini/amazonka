{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. This action is throttled at one
-- request per second. POST / HTTP/1.1 Date: Sat, 12 May 2012 05:21:58 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=hQJj2pxypqJHQgU/BW1EZGUiNIYGhkQDf7tI6UgQ2qw=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 151
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailIdentity
-- &EmailAddress=user%40domain.com &Timestamp=2012-05-12T05%3A21%3A58.000Z
-- &Version=2010-12-01 47e0ef1a-9bf2-11e1-9279-0100e8cf109a.
module Network.AWS.SES.VerifyEmailIdentity where

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
verifyEmailIdentity :: Text
                    -> VerifyEmailIdentity
verifyEmailIdentity p1 = undefined $ VerifyEmailIdentity
    { veirEmailAddress = p1
    }

data VerifyEmailIdentity = VerifyEmailIdentity
    { veirEmailAddress :: !Text
      -- ^ The email address to be verified.
    } deriving (Eq, Show, Generic)

instance ToQuery VerifyEmailIdentity

instance AWSRequest VerifyEmailIdentity where
    type Er VerifyEmailIdentity = SESError
    type Rs VerifyEmailIdentity = VerifyEmailIdentityResponse
    request = getQuery service "VerifyEmailIdentity"

data VerifyEmailIdentityResponse = VerifyEmailIdentityResponse
    deriving (Eq, Show, Generic)

instance FromXML VerifyEmailIdentityResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "VerifyEmailIdentityResponse"
        :| ["VerifyEmailIdentityResult"]
