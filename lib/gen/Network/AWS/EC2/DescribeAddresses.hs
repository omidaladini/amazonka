{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeAddresses operation lists elastic IP addresses assigned to your
-- account.
module Network.AWS.EC2.DescribeAddresses where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeAddresses = DescribeAddresses
    { darAllocationIds :: [Text]
      -- ^ FIXME: Missing documentation
    , darDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , darFilters :: [Filter]
      -- ^ A list of filters used to match properties for Addresses. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , darPublicIps :: [Text]
      -- ^ The optional list of Elastic IP addresses to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAddresses

instance AWSRequest DescribeAddresses where
    type Er DescribeAddresses = EC2Error
    type Rs DescribeAddresses = DescribeAddressesResponse
    request = v2Query service GET "DescribeAddresses"

data DescribeAddressesResponse = DescribeAddressesResponse
    { darrsAddresses :: [Address]
      -- ^ The list of Elastic IPs.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAddressesResponse where
    fromXMLOptions = xmlOptions
