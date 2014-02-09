{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CopySnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CopySnapshot
module Network.AWS.EC2.CopySnapshot where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
copySnapshot :: Text
             -> Text
             -> CopySnapshot
copySnapshot p1 p2 = CopySnapshot
    { csSourceRegion = p1
    , csSourceSnapshotId = p2
    , csDescription = Nothing
    , csDryRun = Nothing
    }

data CopySnapshot = CopySnapshot
    { csDescription :: Maybe Text
    , csDryRun :: Maybe Bool
    , csSourceRegion :: !Text
    , csSourceSnapshotId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CopySnapshot

instance AWSRequest CopySnapshot where
    type Er CopySnapshot = EC2Error
    type Rs CopySnapshot = CopySnapshotResponse
    request = getQuery service "CopySnapshot"

data CopySnapshotResponse = CopySnapshotResponse
    { csrSnapshotId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions
