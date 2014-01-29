{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.ListBuckets where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.Conduit
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.S3.Service
import Network.AWS.S3.Types

-- | Convenience method utilising default fields where applicable.
listBuckets :: AWS (Either S3Error ListBucketsResponse)
listBuckets = undefined ListBuckets

type GetService = ListBuckets
type GetServiceResponse = ListBucketsResponse

data ListBuckets = ListBuckets
    deriving (Generic)

instance ToHeaders ListBuckets

instance ToPath ListBuckets where
    toPath = const "/"

instance ToQuery ListBuckets where
    toQuery = const mempty

instance AWSRequest ListBuckets where
    type Er ListBuckets = S3Error
    type Rs ListBuckets = ListBucketsResponse
    request  = getS3 service
    response = undefined

data ListBucketsResponse = ListBucketsResponse
    { lboBuckets :: [Bucket]
    , lboOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)

instance FromXML ListBucketsResponse where
    fromXMLOptions = xmlOptions
