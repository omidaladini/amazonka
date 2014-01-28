{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or remove permission settings for the specified snapshot.
module Network.AWS.EC2.ModifySnapshotAttribute where

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

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { msarAttribute :: Maybe SnapshotAttributeName
      -- ^ The name of the attribute being modified. Available attribute names:
      -- createVolumePermission.
    , msarCreateVolumePermission :: Maybe CreateVolumePermissionModifications
    , msarDryRun :: Maybe Bool
    , msarGroupNames :: [Text]
      -- ^ The AWS group names to add to or remove from the list of groups that have
      -- permission to create EBS volumes from the specified snapshot. Currently
      -- supports "all". Only valid when the createVolumePermission attribute is
      -- being modified.
    , msarOperationType :: Maybe Text
      -- ^ The operation to perform on the attribute. Available operation names: add,
      -- remove.
    , msarSnapshotId :: !Text
      -- ^ The ID of the EBS snapshot whose attributes are being modified.
    , msarUserIds :: [Text]
      -- ^ The AWS user IDs to add to or remove from the list of users that have
      -- permission to create EBS volumes from the specified snapshot. Currently
      -- supports "all". Only valid when the createVolumePermission attribute is
      -- being modified.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifySnapshotAttribute

instance AWSRequest ModifySnapshotAttribute where
    type Er ModifySnapshotAttribute = EC2Error
    type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse
    request = getQuery service "ModifySnapshotAttribute"

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifySnapshotAttributeResponse where
    fromXMLOptions = xmlOptions