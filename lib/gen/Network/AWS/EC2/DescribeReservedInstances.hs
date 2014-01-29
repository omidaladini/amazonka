{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedInstances operation describes Reserved Instances that
-- were purchased for use with your account.
module Network.AWS.EC2.DescribeReservedInstances where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeReservedInstances :: DescribeReservedInstances
describeReservedInstances = DescribeReservedInstances
    { drirDryRun = Nothing
    , drirFilters = []
    , drirOfferingType = Nothing
    , drirReservedInstancesIds = []
    }

data DescribeReservedInstances = DescribeReservedInstances
    { drirDryRun :: Maybe Bool
    , drirFilters :: [Filter]
      -- ^ A list of filters used to match properties for ReservedInstances. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , drirOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , drirReservedInstancesIds :: [Text]
      -- ^ The optional list of Reserved Instance IDs to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstances

instance AWSRequest DescribeReservedInstances where
    type Er DescribeReservedInstances = EC2Error
    type Rs DescribeReservedInstances = DescribeReservedInstancesResponse
    request = getQuery service "DescribeReservedInstances"

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { drirrsReservedInstances :: [ReservedInstances]
      -- ^ The list of described Reserved Instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesResponse where
    fromXMLOptions = xmlOptions
