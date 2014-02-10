{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DeleteNetworkInterface
module Network.AWS.EC2.DeleteNetworkInterface where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteNetworkInterface :: Text
                       -> DeleteNetworkInterface
deleteNetworkInterface p1 = DeleteNetworkInterface
    { dnieNetworkInterfaceId = p1
    , dnieDryRun = Nothing
    }

data DeleteNetworkInterface = DeleteNetworkInterface
    { dnieDryRun :: Maybe Bool
    , dnieNetworkInterfaceId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteNetworkInterface

instance AWSRequest DeleteNetworkInterface where
    type Er DeleteNetworkInterface = EC2Error
    type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse
    request  = postQuery service "DeleteNetworkInterface"
    response = responseXML

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteNetworkInterfaceResponse"
