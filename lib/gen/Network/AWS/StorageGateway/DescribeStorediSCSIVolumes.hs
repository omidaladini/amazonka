{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns description of the gateway volumes specified in the
-- request. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume ARNs. Example Request The following example shows a
-- request that returns a description of a volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeStorediSCSIVolumes { "VolumeARNs":
-- ["arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"]
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 507 {
-- "StorediSCSIVolumes": [ { "VolumeiSCSIAttributes": { "ChapEnabled": true,
-- "NetworkInterfaceId": "10.243.43.207", "NetworkInterfacePort": 3260,
-- "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }, "PreservedExistingData": false, "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/myg
-- ateway/volume/vol-1122AABB", "VolumeDiskId":
-- "pci-0000:03:00.0-scsi-0:0:0:0", "VolumeId": "vol-1122AABB",
-- "VolumeProgress": 23.7, "VolumeSizeInBytes": 1099511627776, "VolumeStatus":
-- "BOOTSTRAPPING" } ] }.
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes where

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

-- | Convenience method utilising default fields where applicable.
describeStorediSCSIVolumes :: [Text]
                           -> AWS (Either StorageGatewayError DescribeStorediSCSIVolumesResponse)
describeStorediSCSIVolumes p1 = undefined $ DescribeStorediSCSIVolumes
    { dsscsiviVolumeARNs = p1
    }

data DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes
    { dsscsiviVolumeARNs :: [Text]
      -- ^ An array of strings where each string represents the Amazon Resource Name
      -- (ARN) of a stored volume. All of the specified stored volumes must from the
      -- same gateway. Use ListVolumes to get volume ARNs for a gateway.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeStorediSCSIVolumes

instance AWSRequest DescribeStorediSCSIVolumes where
    type Er DescribeStorediSCSIVolumes = StorageGatewayError
    type Rs DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumesResponse
    request  = getJSON service
    response = responseJSON

data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse
    { dsscsivirsStorediSCSIVolumes :: [StorediSCSIVolumeInformation]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeStorediSCSIVolumesResponse
