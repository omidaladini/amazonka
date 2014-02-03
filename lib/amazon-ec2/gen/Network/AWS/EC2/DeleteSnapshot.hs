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

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSnapshot :: Text
               -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { dsuSnapshotId = p1
    , dsuDryRun = Nothing
    }

data DeleteSnapshot = DeleteSnapshot
    { dsuDryRun :: Maybe Bool
    , dsuSnapshotId :: !Text
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
    fromXMLRoot    = fromRoot DeleteSnapshotResponse
