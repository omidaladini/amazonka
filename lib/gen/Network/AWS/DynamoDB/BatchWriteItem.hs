{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.BatchWriteItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The BatchWriteItem operation puts or deletes multiple items in one or more
-- tables. A single call to BatchWriteItem can write up to 1 MB of data, which
-- can comprise as many as 25 put or delete requests. Individual items to be
-- written can be as large as 64 KB. BatchWriteItem cannot update items. To
-- update items, use the UpdateItem API. The individual PutItem and DeleteItem
-- operations specified in BatchWriteItem are atomic; however BatchWriteItem
-- as a whole is not. If any requested operations fail because the table's
-- provisioned throughput is exceeded or an internal processing failure
-- occurs, the failed operations are returned in the UnprocessedItems response
-- parameter. You can investigate and optionally resend the requests.
-- Typically, you would call BatchWriteItem in a loop. Each iteration would
-- check for unprocessed items and submit a new BatchWriteItem request with
-- those unprocessed items until all items have been processed. To write one
-- item, you can use the PutItem operation; to delete one item, you can use
-- the DeleteItem operation. With BatchWriteItem, you can efficiently write or
-- delete large amounts of data, such as from Amazon Elastic MapReduce (EMR),
-- or copy data from another database into Amazon DynamoDB. In order to
-- improve performance with these large-scale operations, BatchWriteItem does
-- not behave in the same way as individual PutItem and DeleteItem calls would
-- For example, you cannot specify conditions on individual put and delete
-- requests, and BatchWriteItem does not return deleted items in the response.
-- If you use a programming language that supports concurrency, such as Java,
-- you can use threads to write items in parallel. Your application must
-- include the necessary logic to manage the threads. With languages that
-- don't support threading, such as PHP, BatchWriteItem will write or delete
-- the specified items one at a time. In both situations, BatchWriteItem
-- provides an alternative where the API performs the specified put and delete
-- operations in parallel, giving you the power of the thread pool approach
-- without having to introduce complexity into your application. Parallel
-- processing reduces latency, but each specified put and delete request
-- consumes the same number of write capacity units whether it is processed in
-- parallel or not. Delete operations on nonexistent items consume one write
-- capacity unit. If one or more of the following is true, Amazon DynamoDB
-- rejects the entire batch write operation: One or more tables specified in
-- the BatchWriteItem request does not exist. Primary key attributes specified
-- on an item in the request do not match those in the corresponding table's
-- primary key schema. You try to perform multiple operations on the same item
-- in the same BatchWriteItem request. For example, you cannot put and delete
-- the same item in the same BatchWriteItem request. The total request size
-- exceeds 1 MB. Any individual item in a batch exceeds 64 KB. Multiple
-- Operations on One Table This example writes several items to the Forum
-- table. The response shows that the final put operation failed, possibly
-- because the application exceeded the provisioned throughput on the table.
-- The UnprocessedItems object shows the unsuccessful put request. The
-- application can call BatchWriteItem again to address such unprocessed
-- requests. { "UnprocessedItems": { "Forum": [ { "PutRequest": { "Item": {
-- "Name": { "S": "Amazon ElastiCache" }, "Category": { "S": "Amazon Web
-- Services" } } } } ] }, "ConsumedCapacity": [ { "TableName": "Forum",
-- "CapacityUnits": 3 } ] }.
module Network.AWS.DynamoDB.BatchWriteItem where

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
batchWriteItem :: HashMap Text [WriteRequest]
               -> BatchWriteItem
batchWriteItem p1 = undefined $ BatchWriteItem
    { bwiiRequestItems = p1
    , bwiiReturnConsumedCapacity = Nothing
    , bwiiReturnItemCollectionMetrics = Nothing
    }

data BatchWriteItem = BatchWriteItem
    { bwiiRequestItems :: HashMap Text [WriteRequest]
      -- ^ A map of one or more table names and, for each table, a list of operations
      -- to be performed (DeleteRequest or PutRequest). Each element in the map
      -- consists of the following: DeleteRequest - Perform a DeleteItem operation
      -- on the specified item. The item to be deleted is identified by a Key
      -- subelement: Key - A map of primary key attribute values that uniquely
      -- identify the item. Each entry in this map consists of an attribute name and
      -- an attribute value. PutRequest - Perform a PutItem operation on the
      -- specified item. The item to be put is identified by an Item subelement:
      -- Item - A map of attributes and their values. Each entry in this map
      -- consists of an attribute name and an attribute value. Attribute values must
      -- not be null; string and binary type attributes must have lengths greater
      -- than zero; and set type attributes must not be empty. Requests that contain
      -- empty values will be rejected with a ValidationException. If you specify
      -- any attributes that are part of an index key, then the data types for those
      -- attributes must match those of the schema in the table's attribute
      -- definition.
    , bwiiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for tables and
      -- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
      -- indexes. If set to NONE (the default), ConsumedCapacity is not included in
      -- the response.
    , bwiiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
      -- ^ If set to SIZE, statistics about item collections, if any, that were
      -- modified during the operation are returned in the response. If set to NONE
      -- (the default), no statistics are returned.
    } deriving (Eq, Show, Generic)

instance ToJSON BatchWriteItem

instance AWSRequest BatchWriteItem where
    type Er BatchWriteItem = DynamoDBError
    type Rs BatchWriteItem = BatchWriteItemResponse
    request  = getJSON service
    response = responseJSON

data BatchWriteItemResponse = BatchWriteItemResponse
    { bwiirsConsumedCapacity :: [ConsumedCapacity]
      -- ^ The capacity units consumed by the operation. Each element consists of:
      -- TableName - The table that consumed the provisioned throughput.
      -- CapacityUnits - The total number of capacity units consumed.
    , bwiirsItemCollectionMetrics :: HashMap Text [ItemCollectionMetrics]
      -- ^ A list of tables that were processed by BatchWriteItem and, for each table,
      -- information about any item collections that were affected by individual
      -- DeleteItem or PutItem operations. Each entry consists of the following
      -- subelements: ItemCollectionKey - The hash key value of the item collection.
      -- This is the same as the hash key of the item. SizeEstimateRange - An
      -- estimate of item collection size, expressed in GB. This is a two-element
      -- array containing a lower bound and an upper bound for the estimate. The
      -- estimate includes the size of all the items in the table, plus the size of
      -- all attributes projected into all of the local secondary indexes on the
      -- table. Use this estimate to measure whether a local secondary index is
      -- approaching its size limit. The estimate is subject to change over time;
      -- therefore, do not rely on the precision or accuracy of the estimate.
    , bwiirsUnprocessedItems :: HashMap Text [WriteRequest]
      -- ^ A map of tables and requests against those tables that were not processed.
      -- The UnprocessedKeys value is in the same form as RequestItems, so you can
      -- provide this value directly to a subsequent BatchGetItem operation. For
      -- more information, see RequestItems in the Request Parameters section. Each
      -- UnprocessedItems entry consists of a table name and, for that table, a list
      -- of operations to perform (DeleteRequest or PutRequest). DeleteRequest -
      -- Perform a DeleteItem operation on the specified item. The item to be
      -- deleted is identified by a Key subelement: Key - A map of primary key
      -- attribute values that uniquely identify the item. Each entry in this map
      -- consists of an attribute name and an attribute value. PutRequest - Perform
      -- a PutItem operation on the specified item. The item to be put is identified
      -- by an Item subelement: Item - A map of attributes and their values. Each
      -- entry in this map consists of an attribute name and an attribute value.
      -- Attribute values must not be null; string and binary type attributes must
      -- have lengths greater than zero; and set type attributes must not be empty.
      -- Requests that contain empty values will be rejected with a
      -- ValidationException. If you specify any attributes that are part of an
      -- index key, then the data types for those attributes must match those of the
      -- schema in the table's attribute definition.
    } deriving (Eq, Show, Generic)

instance FromJSON BatchWriteItemResponse
