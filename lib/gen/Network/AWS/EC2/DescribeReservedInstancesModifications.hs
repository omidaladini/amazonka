{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedInstancesModifications operation describes
-- modifications made to Reserved Instances in your account.
module Network.AWS.EC2.DescribeReservedInstancesModifications where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications
    { drimFilters :: [Filter]
      -- ^ A list of filters used to match properties for
      -- ReservedInstancesModifications. For a complete reference to the available
      -- filter keys for this operation, see the Amazon EC2 API reference.
    , drimNextToken :: Maybe Text
      -- ^ A string specifying the next paginated set of results to return.
    , drimReservedInstancesModificationIds :: [Text]
      -- ^ An optional list of Reserved Instances modification IDs to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstancesModifications

instance AWSRequest DescribeReservedInstancesModifications where
    type Er DescribeReservedInstancesModifications = EC2Error
    type Rs DescribeReservedInstancesModifications = DescribeReservedInstancesModificationsResponse
    request = getQuery service "DescribeReservedInstancesModifications"

data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse
    { drimrNextToken :: Maybe Text
      -- ^ The string specifying the next paginated set of results to return.
    , drimrReservedInstancesModifications :: [ReservedInstancesModification]
      -- ^ The list of Reserved Instances modification requests.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesModificationsResponse where
    fromXMLOptions = xmlOptions
