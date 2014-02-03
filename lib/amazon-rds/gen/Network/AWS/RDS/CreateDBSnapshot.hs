{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a DBSnapshot. The source DBInstance must be in "available" state.
-- https://rds.amazonaws.com/ ?Action=CreateDBSnapshot
-- &DBInstanceIdentifier=simcoprod01 &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql creating us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
module Network.AWS.RDS.CreateDBSnapshot where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createDBSnapshot :: Text
                 -> Text
                 -> CreateDBSnapshot
createDBSnapshot p1 p2 = CreateDBSnapshot
    { cdbsnDBInstanceIdentifier = p1
    , cdbsnDBSnapshotIdentifier = p2
    , cdbsnTags = []
    }

data CreateDBSnapshot = CreateDBSnapshot
    { cdbsnDBInstanceIdentifier :: !Text
      -- ^ The DB instance identifier. This is the unique key that identifies a DB
      -- instance. This parameter isn't case sensitive. Constraints: Must contain
      -- from 1 to 63 alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive hyphens.
    , cdbsnDBSnapshotIdentifier :: !Text
      -- ^ The identifier for the DB snapshot. Constraints: Cannot be null, empty, or
      -- blank Must contain from 1 to 255 alphanumeric characters or hyphens First
      -- character must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens Example: my-snapshot-id.
    , cdbsnTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDBSnapshot

instance AWSRequest CreateDBSnapshot where
    type Er CreateDBSnapshot = RDSError
    type Rs CreateDBSnapshot = CreateDBSnapshotResponse
    request = getQuery service "CreateDBSnapshot"

data CreateDBSnapshotResponse = CreateDBSnapshotResponse
    { cdbsnrsDBSnapshot :: Maybe DBSnapshot
      -- ^ Contains the result of a successful invocation of the following actions:
      -- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
      -- element in the DescribeDBSnapshots action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDBSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateDBSnapshotResponse"
        :| ["CreateDBSnapshotResult"]
