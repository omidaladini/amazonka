{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a set of DHCP options that you specify. Amazon VPC returns an error
-- if the set of options you specify is currently associated with a VPC. You
-- can disassociate the set of options by associating either a new set of
-- options or the default options with the VPC.
module Network.AWS.EC2.DeleteDhcpOptions where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteDhcpOptions :: Text
                  -- ^ The ID of the DHCP options set to delete.
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { ddodDhcpOptionsId = p1
    , ddodDryRun = Nothing
    }

data DeleteDhcpOptions = DeleteDhcpOptions
    { ddodDhcpOptionsId :: !Text
      -- ^ The ID of the DHCP options set to delete.
    , ddodDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteDhcpOptions

instance AWSRequest DeleteDhcpOptions where
    type Er DeleteDhcpOptions = EC2Error
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse
    request = getQuery service "DeleteDhcpOptions"

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteDhcpOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDhcpOptionsResponse"
