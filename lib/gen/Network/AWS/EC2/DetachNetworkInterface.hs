{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DetachNetworkInterface
module Network.AWS.EC2.DetachNetworkInterface where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
detachNetworkInterface :: Text
                       -> DetachNetworkInterface
detachNetworkInterface p1 = DetachNetworkInterface
    { dniAttachmentId = p1
    , dniDryRun = Nothing
    , dniForce = Nothing
    }

data DetachNetworkInterface = DetachNetworkInterface
    { dniAttachmentId :: !Text
    , dniDryRun :: Maybe Bool
    , dniForce :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery DetachNetworkInterface

instance AWSRequest DetachNetworkInterface where
    type Er DetachNetworkInterface = EC2Error
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse
    request  = postQuery service "DetachNetworkInterface"
    response = responseXML

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Show, Generic)

instance FromXML DetachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DetachNetworkInterfaceResponse"
