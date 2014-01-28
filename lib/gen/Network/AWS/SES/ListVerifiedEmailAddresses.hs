{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list containing all of the email addresses that have been
-- verified. The ListVerifiedEmailAddresses action is deprecated as of the May
-- 15, 2012 release of Domain Verification. The ListIdentities action is now
-- preferred. This action is throttled at one request per second. POST /
-- HTTP/1.1 Date: Thu, 18 Aug 2011 22:05:09 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=II0+vvDKGMv71vToBwzR6vZ1hxe/VUE8tWEFUNTUqgE=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 108
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=ListVerifiedEmailAddresses
-- &Timestamp=2011-08-18T22%3A05%3A09.000Z% example@amazon.com
-- 3dd50e97-c865-11e0-b235-099eb63d928d.
module Network.AWS.SES.ListVerifiedEmailAddresses where

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

data ListVerifiedEmailAddresses = ListVerifiedEmailAddresses
    deriving (Eq, Show, Generic)

instance ToQuery ListVerifiedEmailAddresses

instance AWSRequest ListVerifiedEmailAddresses where
    type Er ListVerifiedEmailAddresses = SESError
    type Rs ListVerifiedEmailAddresses = ListVerifiedEmailAddressesResponse
    request = getQuery service "ListVerifiedEmailAddresses"

data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse
    { lvearVerifiedEmailAddresses :: [Text]
      -- ^ A list of email addresses that have been verified.
    } deriving (Eq, Show, Generic)

instance FromXML ListVerifiedEmailAddressesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListVerifiedEmailAddressesResponse"
        :| ["ListVerifiedEmailAddressesResult"]
