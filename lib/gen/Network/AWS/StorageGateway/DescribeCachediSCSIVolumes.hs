{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns a description of the gateway volumes specified in
-- the request. This operation is supported only for the gateway-cached volume
-- architecture. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume Amazon Resource Name (ARN). Example Request The following
-- example shows a request that returns a description of a volume. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeCachediSCSIVolumes { "VolumeARNs":
-- ["arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"]
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 664 {
-- "CachediSCSIVolumes": [ { "VolumeiSCSIAttributes": { "ChapEnabled": true,
-- "LunNumber": 0, "NetworkInterfaceId": "10.243.43.207",
-- "NetworkInterfacePort": 3260, "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }, "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeDiskId": "pci-0000:03:00.0-scsi-0:0:0:0", "VolumeId":
-- "vol-1122AABB", "VolumeSizeInBytes": 1099511627776, "VolumeStatus":
-- "AVAILABLE", "VolumeType": "CACHED iSCSI" } ] }.
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes where

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
describeCachediSCSIVolumes :: [Text]
                           -> DescribeCachediSCSIVolumes
describeCachediSCSIVolumes p1 = undefined $ DescribeCachediSCSIVolumes
    { dcscsiviVolumeARNs = p1
    }

data DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes
    { dcscsiviVolumeARNs :: [Text]
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCachediSCSIVolumes

instance AWSRequest DescribeCachediSCSIVolumes where
    type Er DescribeCachediSCSIVolumes = StorageGatewayError
    type Rs DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumesResponse
    request  = getJSON service
    response = responseJSON

data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse
    { dcscsivirsCachediSCSIVolumes :: [CachediSCSIVolumeInformation]
      -- ^ An array of objects where each object contains metadata about one cached
      -- volume.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCachediSCSIVolumesResponse
