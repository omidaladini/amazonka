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

data CancelReservedInstancesListing = CancelReservedInstancesListing
    { crilReservedInstancesListingId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CancelReservedInstancesListing

instance AWSRequest CancelReservedInstancesListing where
    type Er CancelReservedInstancesListing = EC2Error
    type Rs CancelReservedInstancesListing = CancelReservedInstancesListingResponse
    request  = postQuery service "CancelReservedInstancesListing"
    response = responseXML

data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse
    { crilrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic)

instance FromXML CancelReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
