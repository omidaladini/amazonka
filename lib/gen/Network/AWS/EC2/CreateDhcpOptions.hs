{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a set of DHCP options that you can then associate with one or more
-- VPCs, causing all existing and new instances that you launch in those VPCs
-- to use the set of DHCP options. The following table lists the individual
-- DHCP options you can specify. For more information about the options, go to
-- http://www.ietf.org/rfc/rfc2132.txt.
module Network.AWS.EC2.CreateDhcpOptions where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createDhcpOptions :: [DhcpConfiguration]
                  -- ^ A set of one or more DHCP configurations.
                  -> CreateDhcpOptions
createDhcpOptions p1 = CreateDhcpOptions
    { cdoDhcpConfigurations = p1
    , cdoDryRun = Nothing
    }

data CreateDhcpOptions = CreateDhcpOptions
    { cdoDhcpConfigurations :: [DhcpConfiguration]
      -- ^ A set of one or more DHCP configurations.
    , cdoDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDhcpOptions

instance AWSRequest CreateDhcpOptions where
    type Er CreateDhcpOptions = EC2Error
    type Rs CreateDhcpOptions = CreateDhcpOptionsResponse
    request  = postQuery service "CreateDhcpOptions"
    response = responseXML

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { cdorDhcpOptions :: Maybe DhcpOptions
      -- ^ A set of one or more DHCP options.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDhcpOptionsResponse where
    fromXMLOptions = xmlOptions
