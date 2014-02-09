{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your Internet gateways. You can filter the
-- results to return information only about Internet gateways that match
-- criteria you specify. For example, you could get information only about
-- gateways with particular tags. The Internet gateway must match at least one
-- of the specified values for it to be included in the results. You can
-- specify multiple filters (e.g., the Internet gateway is attached to a
-- particular VPC and is tagged with a particular value). The result includes
-- information for a particular Internet gateway only if the gateway matches
-- all your filters. If there's no match, no special message is returned; the
-- response is simply empty. You can use wildcards with the filter values: an
-- asterisk matches zero or more characters, and ? matches exactly one
-- character. You can escape special characters using a backslash before the
-- character. For example, a value of \*amazon\?\\ searches for the literal
-- string *amazon?\.
module Network.AWS.EC2.DescribeInternetGateways where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeInternetGateways = DescribeInternetGateways
    { digsDryRun :: Maybe Bool
    , digsFilters :: [Filter]
      -- ^ A list of filters used to match properties for Internet Gateways. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , digsInternetGatewayIds :: [Text]
      -- ^ One or more Internet gateway IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeInternetGateways

instance AWSRequest DescribeInternetGateways where
    type Er DescribeInternetGateways = EC2Error
    type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse
    request = getQuery service "DescribeInternetGateways"

data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { digsrInternetGateways :: [InternetGateway]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeInternetGatewaysResponse where
    fromXMLOptions = xmlOptions
