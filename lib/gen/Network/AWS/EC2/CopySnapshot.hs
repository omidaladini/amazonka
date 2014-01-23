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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CopySnapshot = CopySnapshot
    { csrDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , csrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , csrSourceRegion :: !Text
      -- ^ FIXME: Missing documentation
    , csrSourceSnapshotId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery CopySnapshot

instance AWSRequest CopySnapshot where
    type Er CopySnapshot = EC2Error
    type Rs CopySnapshot = CopySnapshotResponse
    request = v2Query service GET "CopySnapshot"

data CopySnapshotResponse = CopySnapshotResponse
    { csrrsSnapshotId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions
