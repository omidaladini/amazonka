{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpcs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your VPCs. You can filter the results to return
-- information only about VPCs that match criteria you specify. For example,
-- you could ask to get information about a particular VPC or VPCs (or all
-- your VPCs) only if the VPC's state is available. You can specify multiple
-- filters (e.g., the VPC uses one of several sets of DHCP options, and the
-- VPC's state is available). The result includes information for a particular
-- VPC only if the VPC matches all your filters. If there's no match, no
-- special message is returned; the response is simply empty. The following
-- table shows the available filters.
module Network.AWS.EC2.DescribeVpcs where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVpcs = DescribeVpcs
    { dvsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvsFilters :: [Filter]
      -- ^ A list of filters used to match properties for VPCs. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dvsVpcIds :: [Text]
      -- ^ The ID of a VPC you want information about.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVpcs

instance AWSRequest DescribeVpcs where
    type Er DescribeVpcs = EC2Error
    type Rs DescribeVpcs = DescribeVpcsResponse
    request = v2Query service GET "DescribeVpcs"

data DescribeVpcsResponse = DescribeVpcsResponse
    { dvsrsVpcs :: [Vpc]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVpcsResponse where
    fromXMLOptions = xmlOptions
