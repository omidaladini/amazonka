{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteTable operation deletes a table and all of its items. After a
-- DeleteTable request, the specified table is in the DELETING state until
-- Amazon DynamoDB completes the deletion. If the table is in the ACTIVE
-- state, you can delete it. If a table is in CREATING or UPDATING states,
-- then Amazon DynamoDB returns a ResourceInUseException. If the specified
-- table does not exist, Amazon DynamoDB returns a ResourceNotFoundException.
-- If table is already in the DELETING state, no error is returned. Amazon
-- DynamoDB might continue to accept data read and write operations, such as
-- GetItem and PutItem, on a table in the DELETING state until the table
-- deletion is complete. When you delete a table, any indexes on that table
-- are also deleted. Use the DescribeTable API to check the status of the
-- table. Delete a Table This example deletes the Reply table. {
-- "TableDescription": { "ItemCount": 0, "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Reply", "TableSizeBytes": 0, "TableStatus": "DELETING" }
-- }.
module Network.AWS.DynamoDB.DeleteTable where

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
deleteTable :: Text
            -> DeleteTable
deleteTable p1 = undefined $ DeleteTable
    { dtiTableName = p1
    }

data DeleteTable = DeleteTable
    { dtiTableName :: !Text
      -- ^ The name of the table to delete.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteTable

instance AWSRequest DeleteTable where
    type Er DeleteTable = DynamoDBError
    type Rs DeleteTable = DeleteTableResponse
    request  = getJSON service
    response = responseJSON

data DeleteTableResponse = DeleteTableResponse
    { dtirsTableDescription :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteTableResponse
