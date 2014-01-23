{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateNetworkInterface
module Network.AWS.EC2.CreateNetworkInterface where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateNetworkInterface = CreateNetworkInterface
    { cnirDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , cnirDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , cnirGroups :: [Text]
      -- ^ FIXME: Missing documentation
    , cnirPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , cnirPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ FIXME: Missing documentation
    , cnirSecondaryPrivateIpAddressCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , cnirSubnetId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery CreateNetworkInterface

instance AWSRequest CreateNetworkInterface where
    type Er CreateNetworkInterface = EC2Error
    type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse
    request = v2Query service GET "CreateNetworkInterface"

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { cnirrsNetworkInterface :: Maybe NetworkInterface
      -- ^ Specifies the characteristics of a network interface.
    } deriving (Eq, Show, Generic)

instance FromXML CreateNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
