{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReleaseAddress operation releases an elastic IP address associated with
-- your account. Releasing an IP address automatically disassociates it from
-- any instance with which it is associated. For more information, see
-- DisassociateAddress. After releasing an elastic IP address, it is released
-- to the IP address pool and might no longer be available to your account.
-- Make sure to update your DNS records and any servers or devices that
-- communicate with the address. If you run this operation on an elastic IP
-- address that is already released, the address might be assigned to another
-- account which will cause Amazon EC2 to return an error.
module Network.AWS.EC2.ReleaseAddress where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
releaseAddress :: ReleaseAddress
releaseAddress = ReleaseAddress
    { rarAllocationId = Nothing
    , rarDryRun = Nothing
    , rarPublicIp = Nothing
    }

data ReleaseAddress = ReleaseAddress
    { rarAllocationId :: Maybe Text
      -- ^ The allocation ID that AWS provided when you allocated the address for use
      -- with Amazon VPC.
    , rarDryRun :: Maybe Bool
    , rarPublicIp :: Maybe Text
      -- ^ The elastic IP address that you are releasing from your account.
    } deriving (Eq, Show, Generic)

instance ToQuery ReleaseAddress

instance AWSRequest ReleaseAddress where
    type Er ReleaseAddress = EC2Error
    type Rs ReleaseAddress = ReleaseAddressResponse
    request = getQuery service "ReleaseAddress"

data ReleaseAddressResponse = ReleaseAddressResponse
    deriving (Eq, Show, Generic)

instance FromXML ReleaseAddressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ReleaseAddressResponse
