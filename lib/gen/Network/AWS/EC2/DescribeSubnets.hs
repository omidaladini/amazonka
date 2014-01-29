{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your subnets. You can filter the results to
-- return information only about subnets that match criteria you specify. For
-- example, you could ask to get information about a particular subnet (or
-- all) only if the subnet's state is available. You can specify multiple
-- filters (e.g., the subnet is in a particular VPC, and the subnet's state is
-- available). The result includes information for a particular subnet only if
-- the subnet matches all your filters. If there's no match, no special
-- message is returned; the response is simply empty. The following table
-- shows the available filters.
module Network.AWS.EC2.DescribeSubnets where

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
describeSubnets :: AWS (Either EC2Error DescribeSubnetsResponse)
describeSubnets = undefined $ DescribeSubnets
    { dsrDryRun = Nothing
    , dsrFilters = []
    , dsrSubnetIds = []
    }

data DescribeSubnets = DescribeSubnets
    { dsrDryRun :: Maybe Bool
    , dsrFilters :: [Filter]
      -- ^ A list of filters used to match properties for Subnets. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dsrSubnetIds :: [Text]
      -- ^ A set of one or more subnet IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSubnets

instance AWSRequest DescribeSubnets where
    type Er DescribeSubnets = EC2Error
    type Rs DescribeSubnets = DescribeSubnetsResponse
    request = getQuery service "DescribeSubnets"

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { dsrrsSubnets :: [Subnet]
      -- ^ Contains a set of one or more Subnet instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSubnetsResponse where
    fromXMLOptions = xmlOptions
