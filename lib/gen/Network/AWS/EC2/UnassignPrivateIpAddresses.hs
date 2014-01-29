{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for UnassignPrivateIpAddresses
module Network.AWS.EC2.UnassignPrivateIpAddresses where

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

-- | Convenience method utilising default fields where applicable.
unassignPrivateIpAddresses :: Text
                           -> [Text]
                           -> AWS (Either EC2Error UnassignPrivateIpAddressesResponse)
unassignPrivateIpAddresses p1 p2 = undefined $ UnassignPrivateIpAddresses
    { upiarNetworkInterfaceId = p1
    , upiarPrivateIpAddresses = p2
    }

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { upiarNetworkInterfaceId :: !Text
    , upiarPrivateIpAddresses :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery UnassignPrivateIpAddresses

instance AWSRequest UnassignPrivateIpAddresses where
    type Er UnassignPrivateIpAddresses = EC2Error
    type Rs UnassignPrivateIpAddresses = UnassignPrivateIpAddressesResponse
    request = getQuery service "UnassignPrivateIpAddresses"

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

instance FromXML UnassignPrivateIpAddressesResponse where
    fromXMLOptions = xmlOptions
