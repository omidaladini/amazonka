{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Sat, 12 May 2012 05:18:45 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=OruiFNV26DCZicLDaQmULHGbjbU8MbC/c5aIo/MMIuM=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 115
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=ListIdentities
-- &Timestamp=2012-05-12T05%3A18%3A45.000Z& Version=2010-12-01 example.com
-- user@example.com cacecf23-9bf1-11e1-9279-0100e8cf109a.
module Network.AWS.SES.ListIdentities where

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
listIdentities :: AWS (Either SESError ListIdentitiesResponse)
listIdentities = undefined $ ListIdentities
    { lirIdentityType = Nothing
    , lirMaxItems = Nothing
    , lirNextToken = Nothing
    }

data ListIdentities = ListIdentities
    { lirIdentityType :: Maybe IdentityType
      -- ^ The type of the identities to list. Possible values are "EmailAddress" and
      -- "Domain". If this parameter is omitted, then all identities will be listed.
    , lirMaxItems :: Maybe Int
      -- ^ The maximum number of identities per page. Possible values are 1-100
      -- inclusive.
    , lirNextToken :: Maybe Text
      -- ^ The token to use for pagination.
    } deriving (Eq, Show, Generic)

instance ToQuery ListIdentities

instance AWSRequest ListIdentities where
    type Er ListIdentities = SESError
    type Rs ListIdentities = ListIdentitiesResponse
    request = getQuery service "ListIdentities"

instance AWSPager ListIdentities where
    next rq rs
        | Just x <- lirrsNextToken rs = Just $ rq { lirNextToken = Just x }
        | otherwise = Nothing

data ListIdentitiesResponse = ListIdentitiesResponse
    { lirrsIdentities :: [Text]
      -- ^ A list of identities.
    , lirrsNextToken :: Maybe Text
      -- ^ The token used for pagination.
    } deriving (Eq, Show, Generic)

instance FromXML ListIdentitiesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListIdentitiesResponse"
        :| ["ListIdentitiesResult"]
