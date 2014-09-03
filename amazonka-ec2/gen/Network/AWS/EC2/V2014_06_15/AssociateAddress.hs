{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AssociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates an Elastic IP address with an instance or a network interface.
-- An Elastic IP address is for use in either the EC2-Classic platform or in a
-- VPC. For more information, see Elastic IP Addresses in the Amazon Elastic
-- Compute Cloud User Guide. [EC2-Classic, default VPC] If the Elastic IP
-- address is already associated with a different instance, it is
-- disassociated from that instance and associated with the specified
-- instance. [EC2-VPC] If you don't specify a private IP address, the Elastic
-- IP address is associated with the primary IP address. If the Elastic IP
-- address is already associated with a different instance or a network
-- interface, you get an error unless you allow reassociation. This is an
-- idempotent operation. If you perform the operation more than once, Amazon
-- EC2 doesn't return an error. Example for EC2-Classic This example request
-- associates an Elastic IP address with an instance in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-2ea64347 &amp;PublicIp=192.0.2.1 &amp;AUTHPARAMS
-- &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateAddressResponse&gt; Example
-- for EC2-VPC This example request associates a Elastic IP address with an
-- instance in a VPC. The AllowReassignment parameter allows the Elastic IP
-- address to be associated with the specified instance even if it's already
-- associated with a different instance or a network interface.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-4fd2431a &amp;AllocationId=eipalloc-5723d13e
-- &amp;AllowReassignment=true &amp;AUTHPARAMS &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;associationId&gt;eipassoc-fc5ca095&lt;/associationId&gt;
-- &lt;/AssociateAddressResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AssociateAddress
    (
    -- * Request
      AssociateAddress
    -- ** Request constructor
    , associateAddress
    -- ** Request lenses
    , aatAllowReassociation
    , aatInstanceId
    , aatPublicIp
    , aatAllocationId
    , aatNetworkInterfaceId
    , aatPrivateIpAddress

    -- * Response
    , AssociateAddressResponse
    -- ** Response lenses
    , aauAssociationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AssociateAddress' request.
associateAddress :: AssociateAddress
associateAddress = AssociateAddress
    { _aatAllowReassociation = Nothing
    , _aatInstanceId = Nothing
    , _aatPublicIp = Nothing
    , _aatAllocationId = Nothing
    , _aatNetworkInterfaceId = Nothing
    , _aatPrivateIpAddress = Nothing
    }

data AssociateAddress = AssociateAddress
    { _aatAllowReassociation :: Maybe Bool
      -- ^ [EC2-VPC] Allows an Elastic IP address that is already associated
      -- with an instance or network interface to be re-associated with
      -- the specified instance or network interface. Otherwise, the
      -- operation fails. Default: false.
    , _aatInstanceId :: Maybe Text
      -- ^ The ID of the instance. The operation fails if you specify an
      -- instance ID unless exactly one network interface is attached.
    , _aatPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    , _aatAllocationId :: Maybe Text
      -- ^ [EC2-VPC] The allocation ID. This is required for EC2-VPC.
    , _aatNetworkInterfaceId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the network interface. If the instance has
      -- more than one network interface, you must specify a network
      -- interface ID.
    , _aatPrivateIpAddress :: Maybe Text
      -- ^ [EC2-VPC] The primary or secondary private IP address to
      -- associate with the Elastic IP address. If no private IP address
      -- is specified, the Elastic IP address is associated with the
      -- primary private IP address.
    } deriving (Show, Generic)

-- | [EC2-VPC] Allows an Elastic IP address that is already associated with an
-- instance or network interface to be re-associated with the specified
-- instance or network interface. Otherwise, the operation fails. Default:
-- false.
aatAllowReassociation
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> AssociateAddress
    -> f AssociateAddress
aatAllowReassociation f x =
    (\y -> x { _aatAllowReassociation = y })
       <$> f (_aatAllowReassociation x)
{-# INLINE aatAllowReassociation #-}

-- | The ID of the instance. The operation fails if you specify an instance ID
-- unless exactly one network interface is attached.
aatInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddress
    -> f AssociateAddress
aatInstanceId f x =
    (\y -> x { _aatInstanceId = y })
       <$> f (_aatInstanceId x)
{-# INLINE aatInstanceId #-}

-- | The Elastic IP address.
aatPublicIp
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddress
    -> f AssociateAddress
aatPublicIp f x =
    (\y -> x { _aatPublicIp = y })
       <$> f (_aatPublicIp x)
{-# INLINE aatPublicIp #-}

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
aatAllocationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddress
    -> f AssociateAddress
aatAllocationId f x =
    (\y -> x { _aatAllocationId = y })
       <$> f (_aatAllocationId x)
{-# INLINE aatAllocationId #-}

-- | [EC2-VPC] The ID of the network interface. If the instance has more than
-- one network interface, you must specify a network interface ID.
aatNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddress
    -> f AssociateAddress
aatNetworkInterfaceId f x =
    (\y -> x { _aatNetworkInterfaceId = y })
       <$> f (_aatNetworkInterfaceId x)
{-# INLINE aatNetworkInterfaceId #-}

-- | [EC2-VPC] The primary or secondary private IP address to associate with the
-- Elastic IP address. If no private IP address is specified, the Elastic IP
-- address is associated with the primary private IP address.
aatPrivateIpAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddress
    -> f AssociateAddress
aatPrivateIpAddress f x =
    (\y -> x { _aatPrivateIpAddress = y })
       <$> f (_aatPrivateIpAddress x)
{-# INLINE aatPrivateIpAddress #-}

instance ToQuery AssociateAddress where
    toQuery = genericQuery def

data AssociateAddressResponse = AssociateAddressResponse
    { _aauAssociationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that represents the association of the Elastic
      -- IP address with an instance.
    } deriving (Show, Generic)

-- | [EC2-VPC] The ID that represents the association of the Elastic IP address
-- with an instance.
aauAssociationId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssociateAddressResponse
    -> f AssociateAddressResponse
aauAssociationId f x =
    (\y -> x { _aauAssociationId = y })
       <$> f (_aauAssociationId x)
{-# INLINE aauAssociationId #-}

instance FromXML AssociateAddressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AssociateAddress where
    type Sv AssociateAddress = EC2
    type Rs AssociateAddress = AssociateAddressResponse

    request = post "AssociateAddress"
    response _ = xmlResponse
