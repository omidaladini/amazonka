-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DynamoDB
    (
    -- * Operations
    -- ** PutItem
      module Network.AWS.DynamoDB.PutItem
    -- ** DeleteItem
    , module Network.AWS.DynamoDB.DeleteItem
    -- ** UpdateItem
    , module Network.AWS.DynamoDB.UpdateItem
    -- ** DeleteTable
    , module Network.AWS.DynamoDB.DeleteTable
    -- ** UpdateTable
    , module Network.AWS.DynamoDB.UpdateTable
    -- ** BatchGetItem
    , module Network.AWS.DynamoDB.BatchGetItem
    -- ** DescribeTable
    , module Network.AWS.DynamoDB.DescribeTable
    -- ** GetItem
    , module Network.AWS.DynamoDB.GetItem
    -- ** BatchWriteItem
    , module Network.AWS.DynamoDB.BatchWriteItem
    -- ** ListTables
    , module Network.AWS.DynamoDB.ListTables
    -- ** Scan
    , module Network.AWS.DynamoDB.Scan
    -- ** Query
    , module Network.AWS.DynamoDB.Query
    -- ** CreateTable
    , module Network.AWS.DynamoDB.CreateTable

    -- * Types
    -- ** WriteRequest
    , WriteRequest (..)
    -- ** UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction (..)
    -- ** TableDescription
    , TableDescription (..)
    -- ** PutRequest
    , PutRequest (..)
    -- ** ProvisionedThroughputDescription
    , ProvisionedThroughputDescription (..)
    -- ** ProvisionedThroughput
    , ProvisionedThroughput (..)
    -- ** Projection
    , Projection (..)
    -- ** LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription (..)
    -- ** LocalSecondaryIndex
    , LocalSecondaryIndex (..)
    -- ** KeysAndAttributes
    , KeysAndAttributes (..)
    -- ** KeySchemaElement
    , KeySchemaElement (..)
    -- ** ItemCollectionMetrics
    , ItemCollectionMetrics (..)
    -- ** GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate (..)
    -- ** GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription (..)
    -- ** GlobalSecondaryIndex
    , GlobalSecondaryIndex (..)
    -- ** ExpectedAttributeValue
    , ExpectedAttributeValue (..)
    -- ** DeleteRequest
    , DeleteRequest (..)
    -- ** ConsumedCapacity
    , ConsumedCapacity (..)
    -- ** Condition
    , Condition (..)
    -- ** Capacity
    , Capacity (..)
    -- ** AttributeValueUpdate
    , AttributeValueUpdate (..)
    -- ** AttributeValue
    , AttributeValue (..)
    -- ** AttributeDefinition
    , AttributeDefinition (..)
    -- ** TableStatus
    , TableStatus (..)
    -- ** Select
    , Select (..)
    -- ** ScalarAttributeType
    , ScalarAttributeType (..)
    -- ** ReturnValue
    , ReturnValue (..)
    -- ** ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)
    -- ** ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)
    -- ** ProjectionType
    , ProjectionType (..)
    -- ** KeyType
    , KeyType (..)
    -- ** IndexStatus
    , IndexStatus (..)
    -- ** ComparisonOperator
    , ComparisonOperator (..)
    -- ** AttributeAction
    , AttributeAction (..)

    -- * Errors
    , DynamoDBError (..)
    ) where

import Network.AWS.DynamoDB.Service
import Network.AWS.DynamoDB.Types

import Network.AWS.DynamoDB.PutItem
import Network.AWS.DynamoDB.DeleteItem
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.DeleteTable
import Network.AWS.DynamoDB.UpdateTable
import Network.AWS.DynamoDB.BatchGetItem
import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.GetItem
import Network.AWS.DynamoDB.BatchWriteItem
import Network.AWS.DynamoDB.ListTables
import Network.AWS.DynamoDB.Scan
import Network.AWS.DynamoDB.Query
import Network.AWS.DynamoDB.CreateTable
