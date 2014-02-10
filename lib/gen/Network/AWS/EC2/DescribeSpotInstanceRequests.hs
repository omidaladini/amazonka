{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Spot Instance requests. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current spot instance requests. For
-- conceptual information about Spot Instances, refer to the Amazon Elastic
-- Compute Cloud Developer Guide or Amazon Elastic Compute Cloud User Guide.
-- You can filter the results to return information only about Spot Instance
-- requests that match criteria you specify. For example, you could get
-- information about requests where the Spot Price you specified is a certain
-- value (you can't use greater than or less than comparison, but you can use
-- * and ? wildcards). You can specify multiple values for a filter. A Spot
-- Instance request must match at least one of the specified values for it to
-- be included in the results. You can specify multiple filters (e.g., the
-- Spot Price is equal to a particular value, and the instance type is
-- m1.small). The result includes information for a particular request only if
-- it matches all your filters. If there's no match, no special message is
-- returned; the response is simply empty. You can use wildcards with the
-- filter values: an asterisk matches zero or more characters, and ? matches
-- exactly one character. You can escape special characters using a backslash
-- before the character. For example, a value of \*amazon\?\\ searches for the
-- literal string *amazon?\.
module Network.AWS.EC2.DescribeSpotInstanceRequests where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { dsirDryRun :: Maybe Bool
    , dsirFilters :: [Filter]
      -- ^ A list of filters used to match properties for SpotInstances. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dsirSpotInstanceRequestIds :: [Text]
      -- ^ The ID of the request.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSpotInstanceRequests

instance AWSRequest DescribeSpotInstanceRequests where
    type Er DescribeSpotInstanceRequests = EC2Error
    type Rs DescribeSpotInstanceRequests = DescribeSpotInstanceRequestsResponse
    request  = postQuery service "DescribeSpotInstanceRequests"
    response = responseXML

data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse
    { dsirrSpotInstanceRequestSet :: [SpotInstanceRequest]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions
