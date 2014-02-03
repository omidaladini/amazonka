{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets permission settings for the specified snapshot.
module Network.AWS.EC2.ResetSnapshotAttribute where

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
resetSnapshotAttribute :: SnapshotAttributeName
                       -> Text
                       -> ResetSnapshotAttribute
resetSnapshotAttribute p1 p2 = ResetSnapshotAttribute
    { rsarAttribute = p1
    , rsarSnapshotId = p2
    , rsarDryRun = Nothing
    }

data ResetSnapshotAttribute = ResetSnapshotAttribute
    { rsarAttribute :: !SnapshotAttributeName
      -- ^ The name of the attribute being reset. Available attribute names:
      -- createVolumePermission.
    , rsarDryRun :: Maybe Bool
    , rsarSnapshotId :: !Text
      -- ^ The ID of the snapshot whose attribute is being reset.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetSnapshotAttribute

instance AWSRequest ResetSnapshotAttribute where
    type Er ResetSnapshotAttribute = EC2Error
    type Rs ResetSnapshotAttribute = ResetSnapshotAttributeResponse
    request = getQuery service "ResetSnapshotAttribute"

data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ResetSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ResetSnapshotAttributeResponse
