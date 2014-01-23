{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for AssignPrivateIpAddresses
module Network.AWS.EC2.AssignPrivateIpAddresses where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiarAllowReassignment :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , apiarNetworkInterfaceId :: !Text
      -- ^ FIXME: Missing documentation
    , apiarPrivateIpAddresses :: [Text]
      -- ^ FIXME: Missing documentation
    , apiarSecondaryPrivateIpAddressCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery AssignPrivateIpAddresses

instance AWSRequest AssignPrivateIpAddresses where
    type Er AssignPrivateIpAddresses = EC2Error
    type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse
    request = v2Query service GET "AssignPrivateIpAddresses"

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

instance FromXML AssignPrivateIpAddressesResponse where
    fromXMLOptions = xmlOptions
