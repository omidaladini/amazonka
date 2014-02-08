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
    { mvarVolumeId = p1
    , mvarAutoEnableIO = Nothing
    , mvarDryRun = Nothing
    }

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { mvarAutoEnableIO :: Maybe Bool
    , mvarDryRun :: Maybe Bool
    , mvarVolumeId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyVolumeAttribute

instance AWSRequest ModifyVolumeAttribute where
    type Er ModifyVolumeAttribute = EC2Error
    type Rs ModifyVolumeAttribute = ModifyVolumeAttributeResponse
    request = getQuery service "ModifyVolumeAttribute"

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyVolumeAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyVolumeAttributeResponse"
