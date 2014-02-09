{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a previously created volume. Once successfully deleted, a new
-- volume can be created with the same name.
module Network.AWS.EC2.DeleteVolume where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteVolume :: Text
             -- ^ The ID of the EBS volume to delete.
             -> DeleteVolume
deleteVolume p1 = DeleteVolume
    { dvhVolumeId = p1
    , dvhDryRun = Nothing
    }

data DeleteVolume = DeleteVolume
    { dvhDryRun :: Maybe Bool
    , dvhVolumeId :: !Text
      -- ^ The ID of the EBS volume to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVolume

instance AWSRequest DeleteVolume where
    type Er DeleteVolume = EC2Error
    type Rs DeleteVolume = DeleteVolumeResponse
    request = getQuery service "DeleteVolume"

data DeleteVolumeResponse = DeleteVolumeResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVolumeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVolumeResponse"
