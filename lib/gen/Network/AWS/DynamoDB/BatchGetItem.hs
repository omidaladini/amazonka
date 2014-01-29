{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.BatchGetItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The BatchGetItem operation returns the attributes of one or more items from
-- one or more tables. You identify requested items by primary key. A single
-- operation can retrieve up to 1 MB of data, which can comprise as many as
-- 100 items. BatchGetItem will return a partial result if the response size
-- limit is exceeded, the table's provisioned throughput is exceeded, or an
-- internal processing failure occurs. If a partial result is returned, the
-- operation returns a value for UnprocessedKeys. You can use this value to
-- retry the operation starting with the next item to get. For example, if you
-- ask to retrieve 100 items, but each individual item is 50 KB in size, the
-- system returns 20 items (1 MB) and an appropriate UnprocessedKeys value so
-- you can get the next page of results. If desired, your application can
-- include its own logic to assemble the pages of results into one dataset. If
-- no items can be processed because of insufficient provisioned throughput on
-- each of the tables involved in the request, BatchGetItem throws
-- ProvisionedThroughputExceededException. By default, BatchGetItem performs
-- eventually consistent reads on every table in the request. If you want
-- strongly consistent reads instead, you can set ConsistentRead to true for
-- any or all tables. In order to minimize response latency, BatchGetItem
-- fetches items in parallel. When designing your application, keep in mind
-- that Amazon DynamoDB does not return attributes in any particular order. To
-- help parse the response by item, include the primary key values for the
-- items in your request in the AttributesToGet parameter. If a requested item
-- does not exist, it is not returned in the result. Requests for nonexistent
-- items consume the minimum read capacity units according to the type of
-- read. For more information, see Capacity Units Calculations in the Amazon
-- DynamoDB Developer Guide. Retrieve Items From Multiple Tables The following
-- sample requests attributes from two different tables. { "Responses": {
-- "Forum": [ { "Name":{ "S":"Amazon DynamoDB" }, "Threads":{ "N":"5" },
-- "Messages":{ "N":"19" }, "Views":{ "N":"35" } }, { "Name":{ "S":"Amazon
-- RDS" }, "Threads":{ "N":"8" }, "Messages":{ "N":"32" }, "Views":{ "N":"38"
-- } }, { "Name":{ "S":"Amazon Redshift" }, "Threads":{ "N":"12" },
-- "Messages":{ "N":"55" }, "Views":{ "N":"47" } } ] "Thread": [ { "Tags":{
-- "SS":["Reads","MultipleUsers"] }, "Message":{ "S":"How many users can read
-- a single data item at a time? Are there any limits?" } } ] },
-- "UnprocessedKeys": { }, "ConsumedCapacity": [ { "TableName": "Forum",
-- "CapacityUnits": 3 }, { "TableName": "Thread", "CapacityUnits": 1 } ] }.
module Network.AWS.DynamoDB.BatchGetItem where

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
batchGetItem :: HashMap Text KeysAndAttributes
             -> BatchGetItem
batchGetItem p1 = undefined $ BatchGetItem
    { bgiiRequestItems = p1
    , bgiiReturnConsumedCapacity = Nothing
    }

data BatchGetItem = BatchGetItem
    { bgiiRequestItems :: HashMap Text KeysAndAttributes
      -- ^ A map of one or more table names and, for each table, the corresponding
      -- primary keys for the items to retrieve. Each table name can be invoked only
      -- once. Each element in the map consists of the following: Keys - An array of
      -- primary key attribute values that define specific items in the table.
      -- AttributesToGet - One or more attributes to be retrieved from the table or
      -- index. By default, all attributes are returned. If a specified attribute is
      -- not found, it does not appear in the result. ConsistentRead - If true, a
      -- strongly consistent read is used; if false (the default), an eventually
      -- consistent read is used.
    , bgiiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for tables and
      -- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
      -- indexes. If set to NONE (the default), ConsumedCapacity is not included in
      -- the response.
    } deriving (Eq, Show, Generic)

instance ToJSON BatchGetItem

instance AWSRequest BatchGetItem where
    type Er BatchGetItem = DynamoDBError
    type Rs BatchGetItem = BatchGetItemResponse
    request  = getJSON service
    response = responseJSON

data BatchGetItemResponse = BatchGetItemResponse
    { bgiirsConsumedCapacity :: [ConsumedCapacity]
      -- ^ The write capacity units consumed by the operation. Each element consists
      -- of: TableName - The table that consumed the provisioned throughput.
      -- CapacityUnits - The total number of capacity units consumed.
    , bgiirsResponses :: HashMap Text [AttributeValue]
      -- ^ A map of table name to a list of items. Each object in Responses consists
      -- of a table name, along with a map of attribute data consisting of the data
      -- type and attribute value.
    , bgiirsUnprocessedKeys :: HashMap Text KeysAndAttributes
      -- ^ A map of tables and their respective keys that were not processed with the
      -- current response. The UnprocessedKeys value is in the same form as
      -- RequestItems, so the value can be provided directly to a subsequent
      -- BatchGetItem operation. For more information, see RequestItems in the
      -- Request Parameters section. Each element consists of: Keys - An array of
      -- primary key attribute values that define specific items in the table.
      -- AttributesToGet - One or more attributes to be retrieved from the table or
      -- index. By default, all attributes are returned. If a specified attribute is
      -- not found, it does not appear in the result. If you are querying an index
      -- and request only attributes that are projected into that index, the
      -- operation will read only the index and not the table. If any of the
      -- requested attributes are not projected into the index, Amazon DynamoDB will
      -- need to fetch each matching item from the table. This extra fetching incurs
      -- additional throughput cost and latency. ConsistentRead - The consistency of
      -- a read operation. If set to true, then a strongly consistent read is used;
      -- otherwise, an eventually consistent read is used.
    } deriving (Eq, Show, Generic)

instance FromJSON BatchGetItemResponse
