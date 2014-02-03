{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.StorageGateway.Service

-- | Lists iSCSI information about a volume.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { vscsiaChapEnabled :: Maybe Bool
      -- ^ Indicates whether mutual CHAP is enabled for the iSCSI target.
    , vscsiaLunNumber :: Maybe Int
      -- ^ The logical disk number.
    , vscsiaNetworkInterfaceId :: Maybe Text
      -- ^ The network interface identifier.
    , vscsiaNetworkInterfacePort :: Maybe Int
      -- ^ The port used to communicate with iSCSI targets.
    , vscsiaTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume target.
    } deriving (Eq, Show, Generic)

instance FromJSON VolumeiSCSIAttributes
instance ToJSON VolumeiSCSIAttributes

-- | FIXME: Type documentation for VolumeRecoveryPointInfo
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { vrpiVolumeARN :: Maybe Text
    , vrpiVolumeRecoveryPointTime :: Maybe Text
    , vrpiVolumeSizeInBytes :: Maybe Integer
    , vrpiVolumeUsageInBytes :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON VolumeRecoveryPointInfo
instance ToJSON VolumeRecoveryPointInfo

-- | FIXME: Type documentation for VolumeInformation
data VolumeInformation = VolumeInformation
    { viVolumeARN :: Maybe Text
    , viVolumeType :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON VolumeInformation
instance ToJSON VolumeInformation

-- | FIXME: Type documentation for VTLDevice
data VTLDevice = VTLDevice
    { vtldDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    , vtldVTLDeviceARN :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON VTLDevice
instance ToJSON VTLDevice

-- | FIXME: Type documentation for TapeRecoveryPointInfo
data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { trpiTapeARN :: Maybe Text
    , trpiTapeRecoveryPointTime :: Maybe UTCTime
    , trpiTapeSizeInBytes :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON TapeRecoveryPointInfo
instance ToJSON TapeRecoveryPointInfo

-- | FIXME: Type documentation for TapeArchive
data TapeArchive = TapeArchive
    { taCompletionTime :: Maybe UTCTime
    , taRetrievedTo :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , taTapeARN :: Maybe Text
    , taTapeBarcode :: Maybe Text
    , taTapeSizeInBytes :: Maybe Integer
    , taTapeStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON TapeArchive
instance ToJSON TapeArchive

-- | FIXME: Type documentation for Tape
data Tape = Tape
    { tProgress :: Maybe Double
    , tTapeARN :: Maybe Text
    , tTapeBarcode :: Maybe Text
    , tTapeSizeInBytes :: Maybe Integer
    , tTapeStatus :: Maybe Text
    , tVTLDevice :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON Tape
instance ToJSON Tape

-- | FIXME: Type documentation for StorediSCSIVolumeInformation
data StorediSCSIVolumeInformation = StorediSCSIVolumeInformation
    { sscsiviPreservedExistingData :: Maybe Bool
    , sscsiviSourceSnapshotId :: Maybe Text
    , sscsiviVolumeARN :: Maybe Text
    , sscsiviVolumeDiskId :: Maybe Text
    , sscsiviVolumeId :: Maybe Text
    , sscsiviVolumeProgress :: Maybe Double
    , sscsiviVolumeSizeInBytes :: Maybe Integer
    , sscsiviVolumeStatus :: Maybe Text
    , sscsiviVolumeType :: Maybe Text
    , sscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    } deriving (Eq, Show, Generic)

instance FromJSON StorediSCSIVolumeInformation
instance ToJSON StorediSCSIVolumeInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { niIpv4Address :: Maybe Text
      -- ^ The Internet Protocol version 4 (IPv4) address of the interface.
    , niIpv6Address :: Maybe Text
      -- ^ The Internet Protocol version 6 (IPv6) address of the interface. Currently
      -- not supported.
    , niMacAddress :: Maybe Text
      -- ^ The Media Access Control (MAC) address of the interface. This is currently
      -- unsupported and will not be returned in output.
    } deriving (Eq, Show, Generic)

instance FromJSON NetworkInterface
instance ToJSON NetworkInterface

-- | FIXME: Type documentation for GatewayInformation
data GatewayInformation = GatewayInformation
    { giGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , giGatewayType :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON GatewayInformation
instance ToJSON GatewayInformation

-- | FIXME: Type documentation for DiskInformation
data DiskInformation = DiskInformation
    { diDiskAllocationResource :: Maybe Text
    , diDiskAllocationType :: Maybe Text
    , diDiskId :: Maybe Text
    , diDiskNode :: Maybe Text
    , diDiskPath :: Maybe Text
    , diDiskSizeInBytes :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON DiskInformation
instance ToJSON DiskInformation

-- | FIXME: Type documentation for DeviceiSCSIAttributes
data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { dscsiaChapEnabled :: Maybe Bool
    , dscsiaNetworkInterfaceId :: Maybe Text
    , dscsiaNetworkInterfacePort :: Maybe Int
    , dscsiaTargetARN :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON DeviceiSCSIAttributes
instance ToJSON DeviceiSCSIAttributes

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
data ChapInfo = ChapInfo
    { ciInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target.
    , ciSecretToAuthenticateInitiator :: Maybe Text
      -- ^ The secret key that the initiator (e.g. Windows client) must provide to
      -- participate in mutual CHAP with the target.
    , ciSecretToAuthenticateTarget :: Maybe Text
      -- ^ The secret key that the target must provide to participate in mutual CHAP
      -- with the initiator (e.g. Windows client).
    , ciTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500
      -- lowercase letters, numbers, periods (.), and hyphens (-).
    } deriving (Eq, Show, Generic)

instance FromJSON ChapInfo
instance ToJSON ChapInfo

-- | FIXME: Type documentation for CachediSCSIVolumeInformation
data CachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { cscsiviSourceSnapshotId :: Maybe Text
    , cscsiviVolumeARN :: Maybe Text
    , cscsiviVolumeId :: Maybe Text
    , cscsiviVolumeProgress :: Maybe Double
    , cscsiviVolumeSizeInBytes :: Maybe Integer
    , cscsiviVolumeStatus :: Maybe Text
    , cscsiviVolumeType :: Maybe Text
    , cscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    } deriving (Eq, Show, Generic)

instance FromJSON CachediSCSIVolumeInformation
instance ToJSON CachediSCSIVolumeInformation
