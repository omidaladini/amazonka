{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about one or more PlacementGroup instances in a user's
-- account.
module Network.AWS.EC2.DescribePlacementGroups where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribePlacementGroups = DescribePlacementGroups
    { dpgsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dpgsFilters :: [Filter]
      -- ^ A list of filters used to match properties for Placement Groups. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dpgsGroupNames :: [Text]
      -- ^ The name of the PlacementGroup.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribePlacementGroups

instance AWSRequest DescribePlacementGroups where
    type Er DescribePlacementGroups = EC2Error
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse
    request = v2Query service GET "DescribePlacementGroups"

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { dpgsrsPlacementGroups :: [PlacementGroup]
      -- ^ Contains information about the specified PlacementGroups.
    } deriving (Eq, Show, Generic)

instance FromXML DescribePlacementGroupsResponse where
    fromXMLOptions = xmlOptions
