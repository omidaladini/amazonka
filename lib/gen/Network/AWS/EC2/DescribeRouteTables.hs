{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeRouteTables
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your route tables. You can filter the results
-- to return information only about tables that match criteria you specify.
-- For example, you could get information only about a table associated with a
-- particular subnet. You can specify multiple values for the filter. The
-- table must match at least one of the specified values for it to be included
-- in the results. You can specify multiple filters (e.g., the table has a
-- particular route, and is associated with a particular subnet). The result
-- includes information for a particular table only if it matches all your
-- filters. If there's no match, no special message is returned; the response
-- is simply empty. You can use wildcards with the filter values: an asterisk
-- matches zero or more characters, and ? matches exactly one character. You
-- can escape special characters using a backslash before the character. For
-- example, a value of \*amazon\?\\ searches for the literal string *amazon?\.
module Network.AWS.EC2.DescribeRouteTables where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeRouteTables = DescribeRouteTables
    { drttDryRun :: Maybe Bool
    , drttFilters :: [Filter]
      -- ^ A list of filters used to match properties for Route Tables. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , drttRouteTableIds :: [Text]
      -- ^ One or more route table IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeRouteTables

instance AWSRequest DescribeRouteTables where
    type Er DescribeRouteTables = EC2Error
    type Rs DescribeRouteTables = DescribeRouteTablesResponse
    request = getQuery service "DescribeRouteTables"

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { drttrRouteTables :: [RouteTable]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeRouteTablesResponse where
    fromXMLOptions = xmlOptions
