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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateReservedInstancesListing = CreateReservedInstancesListing
    { crilsClientToken :: !Text
      -- ^ FIXME: Missing documentation
    , crilsInstanceCount :: !Int
      -- ^ FIXME: Missing documentation
    , crilsPriceSchedules :: [PriceScheduleSpecification]
      -- ^ FIXME: Missing documentation
    , crilsReservedInstancesId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery CreateReservedInstancesListing

instance AWSRequest CreateReservedInstancesListing where
    type Er CreateReservedInstancesListing = EC2Error
    type Rs CreateReservedInstancesListing = CreateReservedInstancesListingResponse
    request = v2Query service GET "CreateReservedInstancesListing"

data CreateReservedInstancesListingResponse = CreateReservedInstancesListingResponse
    { crilsrsReservedInstancesListings :: [ReservedInstancesListing]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML CreateReservedInstancesListingResponse where
    fromXMLOptions = xmlOptions
