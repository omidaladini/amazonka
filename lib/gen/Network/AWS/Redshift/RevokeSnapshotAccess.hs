{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the ability of the specified AWS customer account to restore the
-- specified snapshot. If the account is currently restoring the snapshot, the
-- restore will run to completion. For more information about working with
-- snapshots, go to Amazon Redshift Snapshots in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.RevokeSnapshotAccess where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

data RevokeSnapshotAccess = RevokeSnapshotAccess
    { rsamAccountWithRestoreAccess :: !Text
      -- ^ The identifier of the AWS customer account that can no longer restore the
      -- specified snapshot.
    , rsamSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the snapshot was created from. This parameter
      -- is required if your IAM user has a policy containing a snapshot resource
      -- element that specifies anything other than * for the cluster name.
    , rsamSnapshotIdentifier :: !Text
      -- ^ The identifier of the snapshot that the account can no longer access.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeSnapshotAccess

instance AWSRequest RevokeSnapshotAccess where
    type Er RevokeSnapshotAccess = RedshiftError
    type Rs RevokeSnapshotAccess = RevokeSnapshotAccessResponse
    request = getQuery service "RevokeSnapshotAccess"

data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse
    { rsamrsSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Eq, Show, Generic)

instance FromXML RevokeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RevokeSnapshotAccessResponse"
        :| ["RevokeSnapshotAccessResult"]
