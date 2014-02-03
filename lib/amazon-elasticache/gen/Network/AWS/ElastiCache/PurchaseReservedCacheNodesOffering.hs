{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The PurchaseReservedCacheNodesOffering operation allows you to purchase a
-- reserved cache node offering. https://elasticache.amazonaws.com/
-- ?Action=PurchaseReservedCacheNodesOffering
-- &ReservedCacheNodeId=myreservationID
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &CacheNodeCount=1 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-10T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature= Medium
-- Utilization memcached 438012d3-4052-4cc7-b2e3-8d3372e0e706 payment-pending
-- myreservationID 10 2011-12-18T23:24:56.577Z 31536000 123.0 0.123
-- cache.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering where

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

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
purchaseReservedCacheNodesOffering :: Text
                                   -> PurchaseReservedCacheNodesOffering
purchaseReservedCacheNodesOffering p1 = PurchaseReservedCacheNodesOffering
    { prcnomReservedCacheNodesOfferingId = p1
    , prcnomCacheNodeCount = Nothing
    , prcnomReservedCacheNodeId = Nothing
    }

data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering
    { prcnomCacheNodeCount :: Maybe Int
      -- ^ The number of cache node instances to reserve. Default: 1.
    , prcnomReservedCacheNodeId :: Maybe Text
      -- ^ A customer-specified identifier to track this reservation. Example:
      -- myreservationID.
    , prcnomReservedCacheNodesOfferingId :: !Text
      -- ^ The ID of the reserved cache node offering to purchase. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    } deriving (Eq, Show, Generic)

instance ToQuery PurchaseReservedCacheNodesOffering

instance AWSRequest PurchaseReservedCacheNodesOffering where
    type Er PurchaseReservedCacheNodesOffering = ElastiCacheError
    type Rs PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOfferingResponse
    request = getQuery service "PurchaseReservedCacheNodesOffering"

data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse
    { prcnomrsReservedCacheNode :: Maybe ReservedCacheNode
      -- ^ Represents the output of a PurchaseReservedCacheNodesOffering operation.
    } deriving (Eq, Show, Generic)

instance FromXML PurchaseReservedCacheNodesOfferingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PurchaseReservedCacheNodesOfferingResponse"
        :| ["PurchaseReservedCacheNodesOfferingResult"]
