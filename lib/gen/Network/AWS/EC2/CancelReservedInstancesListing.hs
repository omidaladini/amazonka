{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CancelReservedInstancesListing
module Network.AWS.EC2.CancelReservedInstancesListing where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
cancelReservedInstancesListing :: Text
                               -> CancelReservedInstancesListing
cancelReservedInstancesListing p1 = CancelReservedInstancesListing
    { crilrReservedInstancesListingId = p1
    }

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilrReservedInstancesListingId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CancelReservedInstancesListing

instance AWSRequest CancelReservedInstancesListing where
    type Er CancelReservedInstancesListing = EC2Error
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse
    request = getQuery service "CancelReservedInstancesListing"

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilrrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic)

instance FromXML CancelReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
