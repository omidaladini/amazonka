{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ModifyVolumeAttribute
module Network.AWS.EC2.ModifyVolumeAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyVolumeAttribute :: Text
                      -> ModifyVolumeAttribute
modifyVolumeAttribute p1 = ModifyVolumeAttribute
    { mvaVolumeId = p1
    , mvaAutoEnableIO = Nothing
    , mvaDryRun = Nothing
    }

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { mvaAutoEnableIO :: Maybe Bool
    , mvaDryRun :: Maybe Bool
    , mvaVolumeId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyVolumeAttribute

instance AWSRequest ModifyVolumeAttribute where
    type Er ModifyVolumeAttribute = EC2Error
    type Rs ModifyVolumeAttribute = ModifyVolumeAttributeResponse
    request  = postQuery service "ModifyVolumeAttribute"
    response = responseXML

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyVolumeAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyVolumeAttributeResponse"
