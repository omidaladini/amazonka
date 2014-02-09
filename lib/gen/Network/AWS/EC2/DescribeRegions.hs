{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeRegions operation describes regions zones that are currently
-- available to the account.
module Network.AWS.EC2.DescribeRegions where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeRegions = DescribeRegions
    { drsDryRun :: Maybe Bool
    , drsFilters :: [Filter]
      -- ^ A list of filters used to match properties for Regions. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , drsRegionNames :: [Text]
      -- ^ The optional list of regions to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeRegions

instance AWSRequest DescribeRegions where
    type Er DescribeRegions = EC2Error
    type Rs DescribeRegions = DescribeRegionsResponse
    request = getQuery service "DescribeRegions"

data DescribeRegionsResponse = DescribeRegionsResponse
    { drsrRegions :: [Region]
      -- ^ The list of described Amazon EC2 regions.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeRegionsResponse where
    fromXMLOptions = xmlOptions
