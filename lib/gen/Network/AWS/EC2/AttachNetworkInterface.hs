{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for AttachNetworkInterface
module Network.AWS.EC2.AttachNetworkInterface where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachNetworkInterface :: Int
                       -> Text
                       -> Text
                       -> AttachNetworkInterface
attachNetworkInterface p1 p2 p3 = AttachNetworkInterface
    { aniDeviceIndex = p1
    , aniInstanceId = p2
    , aniNetworkInterfaceId = p3
    , aniDryRun = Nothing
    }

data AttachNetworkInterface = AttachNetworkInterface
    { aniDeviceIndex :: !Int
    , aniDryRun :: Maybe Bool
    , aniInstanceId :: !Text
    , aniNetworkInterfaceId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery AttachNetworkInterface

instance AWSRequest AttachNetworkInterface where
    type Er AttachNetworkInterface = EC2Error
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse
    request  = postQuery service "AttachNetworkInterface"
    response = responseXML

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { anirAttachmentId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML AttachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions