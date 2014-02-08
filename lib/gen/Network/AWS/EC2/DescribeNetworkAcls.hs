{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeNetworkAcls
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about the network ACLs in your VPC. You can filter
-- the results to return information only about ACLs that match criteria you
-- specify. For example, you could get information only the ACL associated
-- with a particular subnet. The ACL must match at least one of the specified
-- values for it to be included in the results. You can specify multiple
-- filters (e.g., the ACL is associated with a particular subnet and has an
-- egress entry that denies traffic to a particular port). The result includes
-- information for a particular ACL only if it matches all your filters. If
-- there's no match, no special message is returned; the response is simply
-- empty. You can use wildcards with the filter values: an asterisk matches
-- zero or more characters, and ? matches exactly one character. You can
-- escape special characters using a backslash before the character. For
-- example, a value of \*amazon\?\\ searches for the literal string *amazon?\.
module Network.AWS.EC2.DescribeNetworkAcls where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeNetworkAcls = DescribeNetworkAcls
    { dnasDryRun :: Maybe Bool
    , dnasFilters :: [Filter]
      -- ^ A list of filters used to match properties for Network ACLs. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dnasNetworkAclIds :: [Text]
      -- ^ One or more network ACL IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkAcls

instance AWSRequest DescribeNetworkAcls where
    type Er DescribeNetworkAcls = EC2Error
    type Rs DescribeNetworkAcls = DescribeNetworkAclsResponse
    request = getQuery service "DescribeNetworkAcls"

data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse
    { dnasrsNetworkAcls :: [NetworkAcl]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkAclsResponse where
    fromXMLOptions = xmlOptions
