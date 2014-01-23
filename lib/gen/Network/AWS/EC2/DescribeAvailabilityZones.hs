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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { dazrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dazrFilters :: [Filter]
      -- ^ A list of filters used to match properties for AvailabilityZones. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dazrZoneNames :: [Text]
      -- ^ A list of the availability zone names to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAvailabilityZones

instance AWSRequest DescribeAvailabilityZones where
    type Er DescribeAvailabilityZones = EC2Error
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse
    request = v2Query service GET "DescribeAvailabilityZones"

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { dazrrsAvailabilityZones :: [AvailabilityZone]
      -- ^ The list of described Amazon EC2 availability zones.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAvailabilityZonesResponse where
    fromXMLOptions = xmlOptions
