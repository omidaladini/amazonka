{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeAvailabilityZones operation describes availability zones that
-- are currently available to the account and their states. Availability zones
-- are not the same across accounts. The availability zone us-east-1a for
-- account A is not necessarily the same as us-east-1a for account B. Zone
-- assignments are mapped independently for each account.
module Network.AWS.EC2.DescribeAvailabilityZones where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazDryRun :: Maybe Bool
    , dazFilters :: [Filter]
      -- ^ A list of filters used to match properties for AvailabilityZones. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dazZoneNames :: [Text]
      -- ^ A list of the availability zone names to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAvailabilityZones

instance AWSRequest DescribeAvailabilityZones where
    type Er DescribeAvailabilityZones = EC2Error
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse
    request  = postQuery service "DescribeAvailabilityZones"
    response = responseXML

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazrAvailabilityZones :: [AvailabilityZone]
      -- ^ The list of described Amazon EC2 availability zones.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAvailabilityZonesResponse where
    fromXMLOptions = xmlOptions
