-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway
    (
    -- * Operations
    -- ** CancelArchival
      module Network.AWS.StorageGateway.CancelArchival
    -- ** CreateStorediSCSIVolume
    , module Network.AWS.StorageGateway.CreateStorediSCSIVolume
    -- ** DescribeChapCredentials
    , module Network.AWS.StorageGateway.DescribeChapCredentials
    -- ** CreateTapes
    , module Network.AWS.StorageGateway.CreateTapes
    -- ** CreateCachediSCSIVolume
    , module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    -- ** AddUploadBuffer
    , module Network.AWS.StorageGateway.AddUploadBuffer
    -- ** UpdateGatewayInformation
    , module Network.AWS.StorageGateway.UpdateGatewayInformation
    -- ** DescribeMaintenanceStartTime
    , module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
    -- ** DescribeWorkingStorage
    , module Network.AWS.StorageGateway.DescribeWorkingStorage
    -- ** DescribeCachediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
    -- ** AddCache
    , module Network.AWS.StorageGateway.AddCache
    -- ** StartGateway
    , module Network.AWS.StorageGateway.StartGateway
    -- ** ShutdownGateway
    , module Network.AWS.StorageGateway.ShutdownGateway
    -- ** UpdateGatewaySoftwareNow
    , module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    -- ** DeleteChapCredentials
    , module Network.AWS.StorageGateway.DeleteChapCredentials
    -- ** UpdateChapCredentials
    , module Network.AWS.StorageGateway.UpdateChapCredentials
    -- ** DescribeUploadBuffer
    , module Network.AWS.StorageGateway.DescribeUploadBuffer
    -- ** DescribeTapes
    , module Network.AWS.StorageGateway.DescribeTapes
    -- ** DescribeStorediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    -- ** CreateSnapshotFromVolumeRecoveryPoint
    , module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
    -- ** RetrieveTapeRecoveryPoint
    , module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
    -- ** DeleteGateway
    , module Network.AWS.StorageGateway.DeleteGateway
    -- ** UpdateMaintenanceStartTime
    , module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
    -- ** DescribeGatewayInformation
    , module Network.AWS.StorageGateway.DescribeGatewayInformation
    -- ** RetrieveTapeArchive
    , module Network.AWS.StorageGateway.RetrieveTapeArchive
    -- ** DescribeTapeArchives
    , module Network.AWS.StorageGateway.DescribeTapeArchives
    -- ** DisableGateway
    , module Network.AWS.StorageGateway.DisableGateway
    -- ** DescribeSnapshotSchedule
    , module Network.AWS.StorageGateway.DescribeSnapshotSchedule
    -- ** DescribeBandwidthRateLimit
    , module Network.AWS.StorageGateway.DescribeBandwidthRateLimit
    -- ** DeleteSnapshotSchedule
    , module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    -- ** UpdateSnapshotSchedule
    , module Network.AWS.StorageGateway.UpdateSnapshotSchedule
    -- ** CreateSnapshot
    , module Network.AWS.StorageGateway.CreateSnapshot
    -- ** CancelRetrieval
    , module Network.AWS.StorageGateway.CancelRetrieval
    -- ** DescribeVTLDevices
    , module Network.AWS.StorageGateway.DescribeVTLDevices
    -- ** DeleteTapeArchive
    , module Network.AWS.StorageGateway.DeleteTapeArchive
    -- ** ListVolumeRecoveryPoints
    , module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
    -- ** ListGateways
    , module Network.AWS.StorageGateway.ListGateways
    -- ** DeleteTape
    , module Network.AWS.StorageGateway.DeleteTape
    -- ** ListLocalDisks
    , module Network.AWS.StorageGateway.ListLocalDisks
    -- ** ListVolumes
    , module Network.AWS.StorageGateway.ListVolumes
    -- ** UpdateBandwidthRateLimit
    , module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
    -- ** AddWorkingStorage
    , module Network.AWS.StorageGateway.AddWorkingStorage
    -- ** DescribeTapeRecoveryPoints
    , module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
    -- ** DeleteBandwidthRateLimit
    , module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    -- ** ActivateGateway
    , module Network.AWS.StorageGateway.ActivateGateway
    -- ** DescribeCache
    , module Network.AWS.StorageGateway.DescribeCache
    -- ** DeleteVolume
    , module Network.AWS.StorageGateway.DeleteVolume

    -- * Types
    -- ** VolumeiSCSIAttributes
    , VolumeiSCSIAttributes (..)
    -- ** VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo (..)
    -- ** VolumeInformation
    , VolumeInformation (..)
    -- ** VTLDevice
    , VTLDevice (..)
    -- ** TapeRecoveryPointInfo
    , TapeRecoveryPointInfo (..)
    -- ** TapeArchive
    , TapeArchive (..)
    -- ** Tape
    , Tape (..)
    -- ** StorediSCSIVolumeInformation
    , StorediSCSIVolumeInformation (..)
    -- ** NetworkInterface
    , NetworkInterface (..)
    -- ** GatewayInformation
    , GatewayInformation (..)
    -- ** DiskInformation
    , DiskInformation (..)
    -- ** DeviceiSCSIAttributes
    , DeviceiSCSIAttributes (..)
    -- ** ChapInfo
    , ChapInfo (..)
    -- ** CachediSCSIVolumeInformation
    , CachediSCSIVolumeInformation (..)

    -- * Errors
    , StorageGatewayError (..)
    ) where

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

import Network.AWS.StorageGateway.CancelArchival
import Network.AWS.StorageGateway.CreateStorediSCSIVolume
import Network.AWS.StorageGateway.DescribeChapCredentials
import Network.AWS.StorageGateway.CreateTapes
import Network.AWS.StorageGateway.CreateCachediSCSIVolume
import Network.AWS.StorageGateway.AddUploadBuffer
import Network.AWS.StorageGateway.UpdateGatewayInformation
import Network.AWS.StorageGateway.DescribeMaintenanceStartTime
import Network.AWS.StorageGateway.DescribeWorkingStorage
import Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
import Network.AWS.StorageGateway.AddCache
import Network.AWS.StorageGateway.StartGateway
import Network.AWS.StorageGateway.ShutdownGateway
import Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
import Network.AWS.StorageGateway.DeleteChapCredentials
import Network.AWS.StorageGateway.UpdateChapCredentials
import Network.AWS.StorageGateway.DescribeUploadBuffer
import Network.AWS.StorageGateway.DescribeTapes
import Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
import Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
import Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
import Network.AWS.StorageGateway.DeleteGateway
import Network.AWS.StorageGateway.UpdateMaintenanceStartTime
import Network.AWS.StorageGateway.DescribeGatewayInformation
import Network.AWS.StorageGateway.RetrieveTapeArchive
import Network.AWS.StorageGateway.DescribeTapeArchives
import Network.AWS.StorageGateway.DisableGateway
import Network.AWS.StorageGateway.DescribeSnapshotSchedule
import Network.AWS.StorageGateway.DescribeBandwidthRateLimit
import Network.AWS.StorageGateway.DeleteSnapshotSchedule
import Network.AWS.StorageGateway.UpdateSnapshotSchedule
import Network.AWS.StorageGateway.CreateSnapshot
import Network.AWS.StorageGateway.CancelRetrieval
import Network.AWS.StorageGateway.DescribeVTLDevices
import Network.AWS.StorageGateway.DeleteTapeArchive
import Network.AWS.StorageGateway.ListVolumeRecoveryPoints
import Network.AWS.StorageGateway.ListGateways
import Network.AWS.StorageGateway.DeleteTape
import Network.AWS.StorageGateway.ListLocalDisks
import Network.AWS.StorageGateway.ListVolumes
import Network.AWS.StorageGateway.UpdateBandwidthRateLimit
import Network.AWS.StorageGateway.AddWorkingStorage
import Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
import Network.AWS.StorageGateway.DeleteBandwidthRateLimit
import Network.AWS.StorageGateway.ActivateGateway
import Network.AWS.StorageGateway.DescribeCache
import Network.AWS.StorageGateway.DeleteVolume
