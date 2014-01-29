{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DBSnapshot. If the snapshot is being copied, the copy operation
-- is terminated. The DBSnapshot must be in the available state to be deleted.
-- https://rds.amazon.com/ &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- 2011-03-11T07:20:24.082Z mysql deleted us-east-1d general-public-license
-- 2010-07-16T00:06:59.107Z 60 simcoprod01 5.1.47 mysnapshot2 manual master
-- 627a43a1-8507-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.DeleteDBSnapshot where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
deleteDBSnapshot :: Text
                 -> AWS (Either RDSError DeleteDBSnapshotResponse)
deleteDBSnapshot p1 = undefined $ DeleteDBSnapshot
    { ddbsnDBSnapshotIdentifier = p1
    }

data DeleteDBSnapshot = DeleteDBSnapshot
    { ddbsnDBSnapshotIdentifier :: !Text
      -- ^ The DBSnapshot identifier. Constraints: Must be the name of an existing DB
      -- snapshot in the available state.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteDBSnapshot

instance AWSRequest DeleteDBSnapshot where
    type Er DeleteDBSnapshot = RDSError
    type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
    request = getQuery service "DeleteDBSnapshot"

data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse
    { ddbsnrsDBSnapshot :: Maybe DBSnapshot
      -- ^ Contains the result of a successful invocation of the following actions:
      -- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
      -- element in the DescribeDBSnapshots action.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteDBSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteDBSnapshotResponse"
        :| ["DeleteDBSnapshotResult"]
