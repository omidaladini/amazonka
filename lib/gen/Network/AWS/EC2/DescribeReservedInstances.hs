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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeReservedInstances = DescribeReservedInstances
    { driDryRun :: Maybe Bool
    , driFilters :: [Filter]
      -- ^ A list of filters used to match properties for ReservedInstances. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , driOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , driReservedInstancesIds :: [Text]
      -- ^ The optional list of Reserved Instance IDs to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstances

instance AWSRequest DescribeReservedInstances where
    type Er DescribeReservedInstances = EC2Error
    type Rs DescribeReservedInstances = DescribeReservedInstancesResponse
    request = getQuery service "DescribeReservedInstances"

data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { drirReservedInstances :: [ReservedInstances]
      -- ^ The list of described Reserved Instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesResponse where
    fromXMLOptions = xmlOptions
