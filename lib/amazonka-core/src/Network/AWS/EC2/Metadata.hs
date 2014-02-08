{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.Metadata
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieve an EC2 instance's local metadata.
module Network.AWS.EC2.Metadata where

import           Control.Applicative
import           Control.Error
import qualified Control.Exception    as EX
import           Control.Monad.Error
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import qualified Data.Text            as Text
import           Data.Text (Text)
import           Network.AWS.Internal
import           Network.AWS.Text
import           Network.HTTP.Conduit
import           Network.HTTP.Types   (status404)

data Dynamic
    = FWS
    -- ^ Value showing whether the customer has enabled detailed one-minute
    -- monitoring in CloudWatch.
    --
    -- Valid values: enabled | disabled.
    | Document
    -- ^ JSON containing instance attributes, such as instance-id,
    -- private IP address, etc.
    | PKCS7
    -- ^ Used to verify the document's authenticity and content against the
    -- signature.
    | Signature

instance ToPath Dynamic where
    toPath FWS       = "fws/instance-monitoring"
    toPath Document  = "instance-identity/document"
    toPath PKCS7     = "instance-identity/pkcs7"
    toPath Signature = "instance-identity/signature"

data Meta
    = AMIId
    -- ^ The AMI ID used to launch the instance.
    | AMILaunchIndex
    -- ^ If you started more than one instance at the same time, this value
    -- indicates the order in which the instance was launched.
    -- The value of the first instance launched is 0.
    | AMIManifestPath
    -- ^ The path to the AMI's manifest file in Amazon S3.
    -- If you used an Amazon EBS-backed AMI to launch the instance,
    -- the returned result is unknown.
    | AncestorAMIIds
    -- ^ The AMI IDs of any instances that were rebundled to create this AMI.
    -- This value will only exist if the AMI manifest file contained an
    -- ancestor-amis key.
    | BlockDevice !Mapping
    -- ^ See: 'Mapping'
    | Hostname
    -- ^ The private hostname of the instance. In cases where multiple network
    -- interfaces are present, this refers to the eth0 device
    -- (the device for which the device number is 0).
    | IAM !Info
    -- ^ See: 'Info'
    | InstanceAction
    -- ^ Notifies the instance that it should reboot in preparation for bundling.
    -- Valid values: none | shutdown | bundle-pending.
    | InstanceId
    -- ^ The ID of this instance.
    | InstanceType
    -- ^ The type of instance.
    --
    -- See: @InstanceType@
    | KernelId
    -- ^ The ID of the kernel launched with this instance, if applicable.
    | LocalHostname
    -- ^ The private DNS hostname of the instance. In cases where multiple
    -- network interfaces are present, this refers to the eth0 device
    -- (the device for which the device number is 0).
    | LocalIPV4
    -- ^ The private IP address of the instance. In cases where multiple network
    -- interfaces are present, this refers to the eth0 device
    -- (the device for which the device number is 0).
    | MAC
    -- ^ The instance's media access control (MAC) address. In cases where
    -- multiple network interfaces are present, this refers to the eth0 device
    -- (the device for which the device number is 0).
    | Network !Text !Interface
    -- ^ See: 'Interface'
    | AvailabilityZone
    -- ^ The Availability Zone in which the instance launched.
    | ProductCodes
    -- ^ Product codes associated with the instance, if any.
    | PublicHostname
    -- ^ The instance's public DNS. If the instance is in a VPC, this category
    -- is only returned if the enableDnsHostnames attribute is set to true.
    -- For more information, see Using DNS with Your VPC.
    | PublicIPV4
    -- ^ The public IP address. If an Elastic IP address is associated with the
    -- instance, the value returned is the Elastic IP address.
    | OpenSSHKey
    -- ^ Public key. Only available if supplied at instance launch time.
    | RAMDiskId
    -- ^ The ID of the RAM disk specified at launch time, if applicable.
    | ReservationId
    -- ^ ID of the reservation.
    | SecurityGroups
    -- ^ The names of the security groups applied to the instance.
      deriving (Eq, Ord, Show)

instance ToPath Meta where
    toPath AMIId            = "ami-id"
    toPath AMILaunchIndex   = "ami-launch-index"
    toPath AMIManifestPath  = "ami-manifest-path"
    toPath AncestorAMIIds   = "ancestor-ami-ids"
    toPath (BlockDevice m)  = "block-device-mapping/" <> toPath m
    toPath Hostname         = "hostname"
    toPath (IAM         m)  = "iam/" <> toPath m
    toPath InstanceAction   = "instance-action"
    toPath InstanceId       = "instance-id"
    toPath InstanceType     = "instance-type"
    toPath KernelId         = "kernel-id"
    toPath LocalHostname    = "local-hostname"
    toPath LocalIPV4        = "local-ipv4"
    toPath MAC              = "mac"
    toPath (Network x   m)  = "network/interfaces/macs/" <> x <> ('/' `Text.cons` toPath m)
    toPath AvailabilityZone = "placement/availability-zone"
    toPath ProductCodes     = "product-codes"
    toPath PublicHostname   = "public-hostname"
    toPath PublicIPV4       = "public-ipv4"
    toPath OpenSSHKey       = "public-keys/0/openssh-key"
    toPath RAMDiskId        = "ramdisk-id"
    toPath ReservationId    = "reservation-id"
    toPath SecurityGroups   = "security-groups"

data Mapping
    = AMI
    -- ^ The virtual device that contains the root/boot file system.
    | EBS !Int
    -- ^ The virtual devices associated with Amazon EBS volumes, if present.
    -- This value is only available in metadata if it is present at launch time.
    -- The N indicates the index of the Amazon EBS volume (such as ebs1 or ebs2).
    | Ephemeral !Int
    -- ^ The virtual devices associated with ephemeral devices, if present.
    -- The N indicates the index of the ephemeral volume.
    | Root
    -- ^ The virtual devices or partitions associated with the root devices,
    -- or partitions on the virtual device, where the root (/ or C:) file system
    -- is associated with the given instance.
    | Swap
    -- ^ The virtual devices associated with swap. Not always present.
      deriving (Eq, Ord, Show)

instance ToPath Mapping where
    toPath AMI           = "ami"
    toPath (EBS       n) = "ebs" <> toText n
    toPath (Ephemeral n) = "ephemeral" <> toText n
    toPath Root          = "root"
    toPath Swap          = "root"

data Interface
    = IDeviceNumber
    -- ^ The device number associated with that interface. Each interface must
    -- have a unique device number. The device number serves as a hint to device
    -- naming in the instance; for example, device-number is 2 for the eth2 device.
    | IIPV4Associations !Text
    -- ^ The private IPv4 addresses that are associated with each public-ip
    -- address and assigned to that interface.
    | ILocalHostname
    -- ^ The interface's local hostname.
    | ILocalIPV4s
    -- ^ The private IP addresses associated with the interface.
    | IMAC
    -- ^ The instance's MAC address.
    | IOwnerId
    -- ^ The ID of the owner of the network interface. In multiple-interface
    -- environments, an interface can be attached by a third party, such as
    -- Elastic Load Balancing. Traffic on an interface is always billed to
    -- the interface owner.
    | IPublicHostname
    -- ^ The interface's public DNS. If the instance is in a VPC, this category
    -- is only returned if the enableDnsHostnames attribute is set to true.
    -- For more information, see Using DNS with Your VPC.
    | IPublicIPV4s
    -- ^ The Elastic IP addresses associated with the interface. There may be
    -- multiple IP addresses on an instance.
    | ISecurityGroups
    -- ^ Security groups to which the network interface belongs. Returned only
    -- for instances launched into a VPC.
    | ISecurityGroupIds
    -- ^ IDs of the security groups to which the network interface belongs.
    -- Returned only for instances launched into a VPC. For more information on
    -- security groups in the EC2-VPC platform, see Security Groups for Your VPC.
    | ISubnetId
    -- ^ The ID of the subnet in which the interface resides. Returned only for
    -- instances launched into a VPC.
    | ISubnetIPV4_CIDRBlock
    -- ^ The CIDR block of the subnet in which the interface resides. Returned
    -- only for instances launched into a VPC.
    | IVPCId
    -- ^ The ID of the VPC in which the interface resides. Returned only for
    -- instances launched into a VPC.
    | IVPCIPV4_CIDRBlock
    -- ^ The CIDR block of the VPC in which the interface resides. Returned only
    -- for instances launched into a VPC.
      deriving (Eq, Ord, Show)

instance ToPath Interface where
    toPath IDeviceNumber          = "device-number"
    toPath (IIPV4Associations ip) = "ipv4-associations/" <> ip
    toPath ILocalHostname         = "local-hostname"
    toPath ILocalIPV4s            = "local-ipv4s"
    toPath IMAC                   = "mac"
    toPath IOwnerId               = "owner-id"
    toPath IPublicHostname        = "public-hostname"
    toPath IPublicIPV4s           = "public-ipv4s"
    toPath ISecurityGroups        = "security-groups"
    toPath ISecurityGroupIds      = "security-group-ids"
    toPath ISubnetId              = "subnet-id"
    toPath ISubnetIPV4_CIDRBlock  = "subnet-ipv4-cidr-block"
    toPath IVPCId                 = "vpc-id"
    toPath IVPCIPV4_CIDRBlock     = "vpc-ipv4-cidr-block"

data Info
    = Info
    -- ^ Returns information about the last time the instance profile was updated,
    -- including the instance's LastUpdated date, InstanceProfileArn,
    -- and InstanceProfileId.
    | SecurityCredentials (Maybe Text)
    -- ^ Where role-name is the name of the IAM role associated with the instance.
    -- Returns the temporary security credentials.
    --
    -- See: 'Auth' for JSON deserialisation.
      deriving (Eq, Ord, Show)

instance ToPath Info where
    toPath Info                    = "info"
    toPath (SecurityCredentials r) = "security-credentials/" <> fromMaybe "" r

ec2 :: (Functor m, MonadIO m) => m Bool
ec2 = fmap isRight
    . runEitherT
    . syncIO
    $ simpleHttp "http://instance-data/latest"

user :: MonadIO m => m (Maybe LBS.ByteString)
user = liftIO $ EX.catch (Just <$> simpleHttp url) ex
  where
    url = "http://169.254.169.254/latest/user-data"

    ex (StatusCodeException s _ _)
        | status404 == s = return Nothing
    ex e                 = EX.throw e

meta :: (Functor m, MonadIO m) => Meta -> m ByteString
meta = get "http://169.254.169.254/latest/meta-data/"

dynamic :: (Functor m, MonadIO m) => Dynamic -> m ByteString
dynamic = get "http://169.254.169.254/latest/dynamic-data/"

get :: (Functor m, MonadIO m, ToPath a) => Text -> a -> m ByteString
get base p = BSH.strip '\n' . LBS.toStrict
    <$> (simpleHttp . Text.unpack $ base <> toPath p)
