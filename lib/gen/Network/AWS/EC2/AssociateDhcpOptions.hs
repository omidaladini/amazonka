{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssociateDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC. Or, associates the default DHCP options with the VPC. The
-- default set consists of the standard EC2 host name, no domain name, no DNS
-- server, no NTP server, and no NetBIOS server or node type. After you
-- associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. For more information
-- about the supported DHCP options and using them with Amazon VPC, go to
-- Using DHCP Options in the Amazon Virtual Private Cloud Developer Guide.
module Network.AWS.EC2.AssociateDhcpOptions where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
associateDhcpOptions :: Text
                     -- ^ The ID of the DHCP options to associate with the VPC. Specify "default" to
                     -- associate the default DHCP options with the VPC.
                     -> Text
                     -- ^ The ID of the VPC to associate the DHCP options with.
                     -> AssociateDhcpOptions
associateDhcpOptions p1 p2 = AssociateDhcpOptions
    { adoDhcpOptionsId = p1
    , adoVpcId = p2
    , adoDryRun = Nothing
    }

data AssociateDhcpOptions = AssociateDhcpOptions
    { adoDhcpOptionsId :: !Text
      -- ^ The ID of the DHCP options to associate with the VPC. Specify "default" to
      -- associate the default DHCP options with the VPC.
    , adoDryRun :: Maybe Bool
    , adoVpcId :: !Text
      -- ^ The ID of the VPC to associate the DHCP options with.
    } deriving (Eq, Show, Generic)

instance ToQuery AssociateDhcpOptions

instance AWSRequest AssociateDhcpOptions where
    type Er AssociateDhcpOptions = EC2Error
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse
    request  = postQuery service "AssociateDhcpOptions"
    response = responseXML

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    deriving (Eq, Show, Generic)

instance FromXML AssociateDhcpOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssociateDhcpOptionsResponse"
