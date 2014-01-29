{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The PurchaseReservedInstancesOffering operation purchases a Reserved
-- Instance for use with your account. With Amazon EC2 Reserved Instances, you
-- purchase the right to launch Amazon EC2 instances for a period of time
-- (without getting insufficient capacity errors) and pay a lower usage rate
-- for the actual time used.
module Network.AWS.EC2.PurchaseReservedInstancesOffering where

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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields where applicable.
purchaseReservedInstancesOffering :: Int
                                  -> Text
                                  -> AWS (Either EC2Error PurchaseReservedInstancesOfferingResponse)
purchaseReservedInstancesOffering p1 p2 = undefined $ PurchaseReservedInstancesOffering
    { priorInstanceCount = p1
    , priorReservedInstancesOfferingId = p2
    , priorDryRun = Nothing
    , priorLimitPrice = Nothing
    }

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { priorDryRun :: Maybe Bool
    , priorInstanceCount :: !Int
      -- ^ The number of Reserved Instances to purchase.
    , priorLimitPrice :: Maybe ReservedInstanceLimitPrice
    , priorReservedInstancesOfferingId :: !Text
      -- ^ The unique ID of the Reserved Instances offering being purchased.
    } deriving (Eq, Show, Generic)

instance ToQuery PurchaseReservedInstancesOffering

instance AWSRequest PurchaseReservedInstancesOffering where
    type Er PurchaseReservedInstancesOffering = EC2Error
    type Rs PurchaseReservedInstancesOffering = PurchaseReservedInstancesOfferingResponse
    request = getQuery service "PurchaseReservedInstancesOffering"

data PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { priorrsReservedInstancesId :: Maybe Text
      -- ^ The unique ID of the Reserved Instances purchased for your account.
    } deriving (Eq, Show, Generic)

instance FromXML PurchaseReservedInstancesOfferingResponse where
    fromXMLOptions = xmlOptions
