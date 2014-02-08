{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateReservedInstancesListing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateReservedInstancesListing
module Network.AWS.EC2.CreateReservedInstancesListing where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createReservedInstancesListing :: Text
                               -> Int
                               -> [PriceScheduleSpecification]
                               -> Text
                               -> CreateReservedInstancesListing
createReservedInstancesListing p1 p2 p3 p4 = CreateReservedInstancesListing
    { crilsClientToken = p1
    , crilsInstanceCount = p2
    , crilsPriceSchedules = p3
    , crilsReservedInstancesId = p4
    }

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { crilsClientToken :: !Text
    , crilsInstanceCount :: !Int
    , crilsPriceSchedules :: [PriceScheduleSpecification]
    , crilsReservedInstancesId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CreateReservedInstancesListing

instance AWSRequest CreateReservedInstancesListing where
    type Er CreateReservedInstancesListing = EC2Error
    type Rs CreateReservedInstancesListing = CreateReservedInstancesListingResponse
    request = getQuery service "CreateReservedInstancesListing"

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { crilsrsReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic)

instance FromXML CreateReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
