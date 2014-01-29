{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Authorizes the specified AWS customer account to restore the specified
-- snapshot. For more information about working with snapshots, go to Amazon
-- Redshift Snapshots in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.AuthorizeSnapshotAccess where

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

-- | Convenience method utilising default fields where applicable.
authorizeSnapshotAccess :: Text
                        -> Text
                        -> AWS (Either RedshiftError AuthorizeSnapshotAccessResponse)
authorizeSnapshotAccess p1 p2 = undefined $ AuthorizeSnapshotAccess
    { asamAccountWithRestoreAccess = p1
    , asamSnapshotIdentifier = p2
    , asamSnapshotClusterIdentifier = Nothing
    }

data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess
    { asamAccountWithRestoreAccess :: !Text
      -- ^ The identifier of the AWS customer account authorized to restore the
      -- specified snapshot.
    , asamSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the snapshot was created from. This parameter
      -- is required if your IAM user has a policy containing a snapshot resource
      -- element that specifies anything other than * for the cluster name.
    , asamSnapshotIdentifier :: !Text
      -- ^ The identifier of the snapshot the account is authorized to restore.
    } deriving (Eq, Show, Generic)

instance ToQuery AuthorizeSnapshotAccess

instance AWSRequest AuthorizeSnapshotAccess where
    type Er AuthorizeSnapshotAccess = RedshiftError
    type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse
    request = getQuery service "AuthorizeSnapshotAccess"

data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { asamrsSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Eq, Show, Generic)

instance FromXML AuthorizeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AuthorizeSnapshotAccessResponse"
        :| ["AuthorizeSnapshotAccessResult"]
