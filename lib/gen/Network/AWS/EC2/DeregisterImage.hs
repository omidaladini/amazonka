{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeregisterImage operation deregisters an AMI. Once deregistered,
-- instances of the AMI can no longer be launched.
module Network.AWS.EC2.DeregisterImage where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deregisterImage :: Text
                -- ^ The ID of the AMI to deregister.
                -> DeregisterImage
deregisterImage p1 = DeregisterImage
    { diImageId = p1
    , diDryRun = Nothing
    }

data DeregisterImage = DeregisterImage
    { diDryRun :: Maybe Bool
    , diImageId :: !Text
      -- ^ The ID of the AMI to deregister.
    } deriving (Eq, Show, Generic)

instance ToQuery DeregisterImage

instance AWSRequest DeregisterImage where
    type Er DeregisterImage = EC2Error
    type Rs DeregisterImage = DeregisterImageResponse
    request  = postQuery service "DeregisterImage"
    response = responseXML

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Show, Generic)

instance FromXML DeregisterImageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeregisterImageResponse"
