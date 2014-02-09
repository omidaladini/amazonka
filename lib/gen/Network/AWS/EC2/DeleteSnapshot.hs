{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the snapshot identified by snapshotId.
module Network.AWS.EC2.DeleteSnapshot where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSnapshot :: Text
               -- ^ The ID of the snapshot to delete.
               -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { dsfSnapshotId = p1
    , dsfDryRun = Nothing
    }

data DeleteSnapshot = DeleteSnapshot
    { dsfDryRun :: Maybe Bool
    , dsfSnapshotId :: !Text
      -- ^ The ID of the snapshot to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSnapshot

instance AWSRequest DeleteSnapshot where
    type Er DeleteSnapshot = EC2Error
    type Rs DeleteSnapshot = DeleteSnapshotResponse
    request = getQuery service "DeleteSnapshot"

data DeleteSnapshotResponse = DeleteSnapshotResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSnapshotResponse"
