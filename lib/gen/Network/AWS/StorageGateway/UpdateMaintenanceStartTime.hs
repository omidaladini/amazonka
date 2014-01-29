{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateMaintenanceStartTime
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates a gateway's weekly maintenance start time
-- information, including day and time of the week. The maintenance time is
-- the time in your gateway's time zone. Example Request The following example
-- shows a request that updates the maintenance start time of mygateway. POST
-- / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateMaintenanceStartTime { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "HourOfDay": 0, "MinuteOfHour": 30, "DayOfWeek": 2 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.UpdateMaintenanceStartTime where

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

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateMaintenanceStartTime :: Int
                           -> Text
                           -> Int
                           -> Int
                           -> UpdateMaintenanceStartTime
updateMaintenanceStartTime p1 p2 p3 p4 = undefined $ UpdateMaintenanceStartTime
    { umstiDayOfWeek = p1
    , umstiGatewayARN = p2
    , umstiHourOfDay = p3
    , umstiMinuteOfHour = p4
    }

data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime
    { umstiDayOfWeek :: !Int
      -- ^ The maintenance start time day of the week.
    , umstiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , umstiHourOfDay :: !Int
      -- ^ The hour component of the maintenance start time represented as hh, where
      -- hh is the hour (00 to 23). The hour of the day is in the time zone of the
      -- gateway.
    , umstiMinuteOfHour :: !Int
      -- ^ The minute component of the maintenance start time represented as mm, where
      -- mm is the minute (00 to 59). The minute of the hour is in the time zone of
      -- the gateway.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateMaintenanceStartTime

instance AWSRequest UpdateMaintenanceStartTime where
    type Er UpdateMaintenanceStartTime = StorageGatewayError
    type Rs UpdateMaintenanceStartTime = UpdateMaintenanceStartTimeResponse
    request  = getJSON service
    response = responseJSON

data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse
    { umstirsGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance FromJSON UpdateMaintenanceStartTimeResponse
