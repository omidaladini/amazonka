{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture. The
-- response includes disk IDs that are configured as cache, and it includes
-- the amount of cache allocated and used. Example Request The following
-- example shows a request to obtain a description of a gateway's working
-- storage. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- Content-Type: application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeCache {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 271 {
-- "CacheAllocationInBytes": 2199023255552, "CacheDirtyPercentage": 0.07,
-- "CacheHitPercentage": 99.68, "CacheMissPercentage": 0.32,
-- "CacheUsedPercentage": 0.07, "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-0:1:0:0" ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.DescribeCache where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

data DescribeCache = DescribeCache
    { dciGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCache

instance AWSRequest DescribeCache where
    type Er DescribeCache = StorageGatewayError
    type Rs DescribeCache = DescribeCacheResponse
    request  = getJSON service
    response = responseJSON

data DescribeCacheResponse = DescribeCacheResponse
    { dcirsCacheAllocatedInBytes :: Maybe Integer
    , dcirsCacheDirtyPercentage :: Maybe Double
    , dcirsCacheHitPercentage :: Maybe Double
    , dcirsCacheMissPercentage :: Maybe Double
    , dcirsCacheUsedPercentage :: Maybe Double
    , dcirsDiskIds :: [Text]
    , dcirsGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCacheResponse