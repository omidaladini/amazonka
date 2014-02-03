{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified email address from the list of verified addresses.
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The DeleteIdentity action is now preferred.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Thu, 18 Aug 2011 22:20:50 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Rxzyd6cQe/YjkV4yoQAZ243OzzNjFgrsclizTKwRIRc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 142
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=DeleteVerifiedEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A20%3A50.000Z
-- 5634af08-c865-11e0-8986-3f99a698f914.
module Network.AWS.SES.DeleteVerifiedEmailAddress where

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
deleteVerifiedEmailAddress :: Text
                           -> DeleteVerifiedEmailAddress
deleteVerifiedEmailAddress p1 = DeleteVerifiedEmailAddress
    { dvearEmailAddress = p1
    }

data DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress
    { dvearEmailAddress :: !Text
      -- ^ An email address to be removed from the list of verified addresses.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVerifiedEmailAddress

instance AWSRequest DeleteVerifiedEmailAddress where
    type Er DeleteVerifiedEmailAddress = SESError
    type Rs DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddressResponse
    request = getQuery service "DeleteVerifiedEmailAddress"

data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVerifiedEmailAddressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteVerifiedEmailAddressResponse
