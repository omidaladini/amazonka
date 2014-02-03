{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the list of AWS Direct Connect locations in the current AWS region.
-- These are the locations that may be selected when calling CreateConnection
-- or CreateInterconnect.
module Network.AWS.DirectConnect.DescribeLocations where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DirectConnect.Service
import Network.AWS.DirectConnect.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations

data DescribeLocations = DescribeLocations
    deriving (Eq, Show, Generic)

instance ToJSON DescribeLocations where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeLocations where
    type Er DescribeLocations = DirectConnectError
    type Rs DescribeLocations = DescribeLocationsResponse
    request  = getJSON service
    response = responseJSON

data DescribeLocationsResponse = DescribeLocationsResponse
    { lLocations :: [Location]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeLocationsResponse where
    fromJSON = genericFromJSON jsonOptions

