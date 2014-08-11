{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified snapshot. When you make periodic snapshots of a
-- volume, the snapshots are incremental, and only the blocks on the device
-- that have changed since your last snapshot are saved in the new snapshot.
-- When you delete a snapshot, only the data not needed for any other snapshot
-- is removed. So regardless of which prior snapshots have been deleted, all
-- active snapshots will have access to all the information needed to restore
-- the volume. You cannot delete a snapshot of the root device of an Amazon
-- EBS volume used by a registered AMI. You must first de-register the AMI
-- before you can delete the snapshot. For more information, see Deleting an
-- Amazon EBS Snapshot in the Amazon Elastic Compute Cloud User Guide. Example
-- This example request deletes the snapshot with the ID snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DeleteSnapshot
-- &amp;SnapshotId.1=snap-1a2b3c4d &amp;AUTHPARAMS &lt;DeleteSnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSnapshotResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteSnapshot where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSnapshot' request.
deleteSnapshot :: Text -- ^ '_dsySnapshotId'
               -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { _dsySnapshotId = p1
    , _dsyDryRun = Nothing
    }

data DeleteSnapshot = DeleteSnapshot
    { _dsySnapshotId :: Text
      -- ^ The ID of the Amazon EBS snapshot.
    , _dsyDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteSnapshot

instance ToQuery DeleteSnapshot where
    toQuery = genericToQuery def

data DeleteSnapshotResponse = DeleteSnapshotResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteSnapshotResponse

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = EC2
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request = post "DeleteSnapshot"
    response _ = nullaryResponse DeleteSnapshotResponse