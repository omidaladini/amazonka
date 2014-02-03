{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on the
-- table. Describe a Table This example describes the Thread table. { "Table":
-- { "AttributeDefinitions": [ { "AttributeName": "ForumName",
-- "AttributeType": "S" }, { "AttributeName": "LastPostDateTime",
-- "AttributeType": "S" }, { "AttributeName": "Subject", "AttributeType": "S"
-- } ], "CreationDateTime": 1.363729002358E9, "ItemCount": 0, "KeySchema": [ {
-- "AttributeName": "ForumName", "KeyType": "HASH" }, { "AttributeName":
-- "Subject", "KeyType": "RANGE" } ], "LocalSecondaryIndexes": [ {
-- "IndexName": "LastPostIndex", "IndexSizeBytes": 0, "ItemCount": 0,
-- "KeySchema": [ { "AttributeName": "ForumName", "KeyType": "HASH" }, {
-- "AttributeName": "LastPostDateTime", "KeyType": "RANGE" } ], "Projection":
-- { "ProjectionType": "KEYS_ONLY" } } ], "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "ACTIVE" }
-- }.
module Network.AWS.DynamoDB.DescribeTable where

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
describeTable :: Text
              -> DescribeTable
describeTable p1 = DescribeTable
    { dtjTableName = p1
    }

data DescribeTable = DescribeTable
    { dtjTableName :: !Text
      -- ^ The name of the table to describe.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTable where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeTable where
    type Er DescribeTable = DynamoDBError
    type Rs DescribeTable = DescribeTableResponse
    request  = getJSON service
    response = responseJSON

data DescribeTableResponse = DescribeTableResponse
    { dtjrsTable :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTableResponse where
    fromJSON = genericFromJSON jsonOptions

