{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Purchases a reserved DB instance offering. https://rds.amazonaws.com/
-- ?Action=PurchaseReservedDBInstancesOffering
-- &ReservedDBInstanceId=myreservationID
-- &ReservedDBInstancesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &DBInstanceCount=1 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-10T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature= Medium
-- Utilization USD mysql 438012d3-4052-4cc7-b2e3-8d3372e0e706 true
-- payment-pending myreservationID 10 2011-12-18T23:24:56.577Z 31536000 123.0
-- 0.123 db.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
purchaseReservedDBInstancesOffering :: Text
                                    -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering p1 = undefined $ PurchaseReservedDBInstancesOffering
    { prdbiomReservedDBInstancesOfferingId = p1
    , prdbiomDBInstanceCount = Nothing
    , prdbiomReservedDBInstanceId = Nothing
    , prdbiomTags = []
    }

data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering
    { prdbiomDBInstanceCount :: Maybe Int
      -- ^ The number of instances to reserve. Default: 1.
    , prdbiomReservedDBInstanceId :: Maybe Text
      -- ^ Customer-specified identifier to track this reservation. Example:
      -- myreservationID.
    , prdbiomReservedDBInstancesOfferingId :: !Text
      -- ^ The ID of the Reserved DB instance offering to purchase. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    , prdbiomTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery PurchaseReservedDBInstancesOffering

instance AWSRequest PurchaseReservedDBInstancesOffering where
    type Er PurchaseReservedDBInstancesOffering = RDSError
    type Rs PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOfferingResponse
    request = getQuery service "PurchaseReservedDBInstancesOffering"

data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse
    { prdbiomrsReservedDBInstance :: Maybe ReservedDBInstance
      -- ^ This data type is used as a response element in the
      -- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
      -- actions.
    } deriving (Eq, Show, Generic)

instance FromXML PurchaseReservedDBInstancesOfferingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PurchaseReservedDBInstancesOfferingResponse"
        :| ["PurchaseReservedDBInstancesOfferingResult"]
