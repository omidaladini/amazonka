{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AssociateAddress operation associates an elastic IP address with an
-- instance. If the IP address is currently assigned to another instance, the
-- IP address is assigned to the new instance. This is an idempotent
-- operation. If you enter it more than once, Amazon EC2 does not return an
-- error.
module Network.AWS.EC2.AssociateAddress where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data AssociateAddress = AssociateAddress
    { aasAllocationId :: Maybe Text
      -- ^ The allocation ID that AWS returned when you allocated the elastic IP
      -- address for use with Amazon VPC.
    , aasAllowReassociation :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , aasDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , aasInstanceId :: Maybe Text
      -- ^ The instance to associate with the IP address.
    , aasNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aasPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aasPublicIp :: Maybe Text
      -- ^ IP address that you are assigning to the instance.
    } deriving (Eq, Show, Generic)

instance ToQuery AssociateAddress

instance AWSRequest AssociateAddress where
    type Er AssociateAddress = EC2Error
    type Rs AssociateAddress = AssociateAddressResponse
    request = v2Query service GET "AssociateAddress"

data AssociateAddressResponse = AssociateAddressResponse
    { aasrsAssociationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML AssociateAddressResponse where
    fromXMLOptions = xmlOptions
