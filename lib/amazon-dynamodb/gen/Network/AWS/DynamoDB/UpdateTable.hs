{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the provisioned throughput for the given table. Setting the
-- throughput for a table helps you manage performance and is part of the
-- provisioned throughput feature of Amazon DynamoDB. The provisioned
-- throughput values can be upgraded or downgraded based on the maximums and
-- minimums listed in the Limits section in the Amazon DynamoDB Developer
-- Guide. The table must be in the ACTIVE state for this operation to succeed.
-- UpdateTable is an asynchronous operation; while executing the operation,
-- the table is in the UPDATING state. While the table is in the UPDATING
-- state, the table still has the provisioned throughput from before the call.
-- The new provisioned throughput setting is in effect only when the table
-- returns to the ACTIVE state after the UpdateTable operation. You cannot
-- add, modify or delete indexes using UpdateTable. Indexes can only be
-- defined at table creation time. Modify Provisioned Write Throughput This
-- example changes both the provisioned read and write throughput of the
-- Thread table to 10 capacity units. { "TableDescription": {
-- "AttributeDefinitions": [ { "AttributeName": "ForumName", "AttributeType":
-- "S" }, { "AttributeName": "LastPostDateTime", "AttributeType": "S" }, {
-- "AttributeName": "Subject", "AttributeType": "S" } ], "CreationDateTime":
-- 1.363801528686E9, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "Subject", "KeyType":
-- "RANGE" } ], "LocalSecondaryIndexes": [ { "IndexName": "LastPostIndex",
-- "IndexSizeBytes": 0, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "LastPostDateTime",
-- "KeyType": "RANGE" } ], "Projection": { "ProjectionType": "KEYS_ONLY" } }
-- ], "ProvisionedThroughput": { "LastIncreaseDateTime": 1.363801701282E9,
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "UPDATING"
-- } }.
module Network.AWS.DynamoDB.UpdateTable where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DynamoDB.Service
import Network.AWS.DynamoDB.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateTable :: Text
            -> UpdateTable
updateTable p1 = UpdateTable
    { utiTableName = p1
    , utiGlobalSecondaryIndexUpdates = []
    , utiProvisionedThroughput = Nothing
    }

data UpdateTable = UpdateTable
    { utiGlobalSecondaryIndexUpdates :: [GlobalSecondaryIndexUpdate]
      -- ^ An array of one or more global secondary indexes on the table, together
      -- with provisioned throughput settings for each index.
    , utiProvisionedThroughput :: Maybe ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified table or
      -- index. The settings can be modified using the UpdateTable operation. For
      -- current minimum and maximum provisioned throughput values, see Limits in
      -- the Amazon DynamoDB Developer Guide.
    , utiTableName :: !Text
      -- ^ The name of the table to be updated.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateTable where
    toJSON = genericToJSON jsonOptions

instance AWSRequest UpdateTable where
    type Er UpdateTable = DynamoDBError
    type Rs UpdateTable = UpdateTableResponse
    request  = getJSON service
    response = responseJSON

data UpdateTableResponse = UpdateTableResponse
    { utirsTableDescription :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Eq, Show, Generic)

instance FromJSON UpdateTableResponse where
    fromJSON = genericFromJSON jsonOptions

