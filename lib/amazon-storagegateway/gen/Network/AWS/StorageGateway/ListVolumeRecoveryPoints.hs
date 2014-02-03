{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists the recovery points for a specified gateway. This
-- operation is supported only for the gateway-cached volume architecture.
-- Each gateway-cached volume has one recovery point. A volume recovery point
-- is a point in time at which all data of the volume is consistent and from
-- which you can create a snapshot. To create a snapshot from a volume
-- recovery point use the CreateSnapshotFromVolumeRecoveryPoint operation.
-- Example Request The following example sends a ListVolumeRecoveryPoints
-- request to take a snapshot of the specified example volume. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.ListVolumeRecoveryPoints { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 137 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "VolumeRecoveryPointInfos": [ { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeRecoveryPointTime": "2012-09-04T21:08:44.627Z", "VolumeSizeInBytes":
-- 536870912000, "VolumeUsageInBytes": 6694048 } ] }.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints where

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
listVolumeRecoveryPoints :: Text
                         -> ListVolumeRecoveryPoints
listVolumeRecoveryPoints p1 = ListVolumeRecoveryPoints
    { lvrpiGatewayARN = p1
    }

data ListVolumeRecoveryPoints = ListVolumeRecoveryPoints
    { lvrpiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance ToJSON ListVolumeRecoveryPoints where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ListVolumeRecoveryPoints where
    type Er ListVolumeRecoveryPoints = StorageGatewayError
    type Rs ListVolumeRecoveryPoints = ListVolumeRecoveryPointsResponse
    request  = getJSON service
    response = responseJSON

data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse
    { lvrpirsGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , lvrpirsVolumeRecoveryPointInfos :: [VolumeRecoveryPointInfo]
    } deriving (Eq, Show, Generic)

instance FromJSON ListVolumeRecoveryPointsResponse where
    fromJSON = genericFromJSON jsonOptions

