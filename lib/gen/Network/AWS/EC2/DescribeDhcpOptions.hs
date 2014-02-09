{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about one or more sets of DHCP options. You can
-- specify one or more DHCP options set IDs, or no IDs (to describe all your
-- sets of DHCP options). The returned information consists of: The DHCP
-- options set ID The options.
module Network.AWS.EC2.DescribeDhcpOptions where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeDhcpOptions = DescribeDhcpOptions
    { ddoDhcpOptionsIds :: [Text]
    , ddoDryRun :: Maybe Bool
    , ddoFilters :: [Filter]
      -- ^ A list of filters used to match properties for DhcpOptions. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDhcpOptions

instance AWSRequest DescribeDhcpOptions where
    type Er DescribeDhcpOptions = EC2Error
    type Rs DescribeDhcpOptions = DescribeDhcpOptionsResponse
    request = getQuery service "DescribeDhcpOptions"

data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { ddorDhcpOptions :: [DhcpOptions]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDhcpOptionsResponse where
    fromXMLOptions = xmlOptions
