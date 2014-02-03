{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DynamoDB.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DynamoDB.Service

-- | Represents an operation to perform - either DeleteItem or PutItem. You can
-- only specify one of these operations, not both, in a single WriteRequest.
-- If you do need to perform both of these operations, you will need to
-- specify two separate WriteRequest objects.
data WriteRequest = WriteRequest
    { wrDeleteRequest :: Maybe DeleteRequest
      -- ^ A request to perform a DeleteItem operation.
    , wrPutRequest :: Maybe PutRequest
      -- ^ A request to perform a PutItem operation.
    } deriving (Eq, Show, Generic)

instance FromJSON WriteRequest
instance ToJSON WriteRequest

-- | The name of a global secondary index, along with the updated provisioned
-- throughput settings that are to be applied to that index.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction
    { ugsiaIndexName :: !Text
      -- ^ The name of the global secondary index to be updated.
    , ugsiaProvisionedThroughput :: ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified table or
      -- index. The settings can be modified using the UpdateTable operation. For
      -- current minimum and maximum provisioned throughput values, see Limits in
      -- the Amazon DynamoDB Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromJSON UpdateGlobalSecondaryIndexAction
instance ToJSON UpdateGlobalSecondaryIndexAction

-- | Represents the properties of a table.
data TableDescription = TableDescription
    { tdAttributeDefinitions :: [AttributeDefinition]
      -- ^ An array of AttributeDefinition objects. Each of these objects describes
      -- one attribute in the table and index key schema. Each AttributeDefinition
      -- object in this array is composed of: AttributeName - The name of the
      -- attribute. AttributeType - The data type for the attribute.
    , tdCreationDateTime :: Maybe UTCTime
      -- ^ The date and time when the table was created, in UNIX epoch time format.
    , tdGlobalSecondaryIndexes :: [GlobalSecondaryIndexDescription]
      -- ^ The global secondary indexes, if any, on the table. Each index is scoped to
      -- a given hash key value. Each element is composed of: IndexName - The name
      -- of the global secondary index. KeySchema - Specifies the complete index key
      -- schema. The attribute names in the key schema must be between 1 and 255
      -- characters (inclusive). The key schema must begin with the same hash key
      -- attribute as the table. Projection - Specifies attributes that are copied
      -- (projected) from the table into the index. These are in addition to the
      -- primary key attributes and index key attributes, which are automatically
      -- projected. Each attribute specification is composed of: ProjectionType -
      -- One of the following: KEYS_ONLY - Only the index and primary keys are
      -- projected into the index. INCLUDE - Only the specified table attributes are
      -- projected into the index. The list of projected attributes are in
      -- NonKeyAttributes. ALL - All of the table attributes are projected into the
      -- index. NonKeyAttributes - A list of one or more non-key attribute names
      -- that are projected into the secondary index. The total count of attributes
      -- specified in NonKeyAttributes, summed across all of the secondary indexes,
      -- must not exceed 20. If you project the same attribute into two different
      -- indexes, this counts as two distinct attributes when determining the total.
      -- ProvisionedThroughput - The provisioned throughput settings for the global
      -- secondary index, consisting of read and write capacity units, along with
      -- data about increases and decreases. IndexSizeBytes - The total size of the
      -- global secondary index, in bytes. Amazon DynamoDB updates this value
      -- approximately every six hours. Recent changes might not be reflected in
      -- this value. ItemCount - The number of items in the global secondary index.
      -- Amazon DynamoDB updates this value approximately every six hours. Recent
      -- changes might not be reflected in this value. If the table is in the
      -- DELETING state, no information about indexes will be returned.
    , tdItemCount :: Maybe Integer
      -- ^ The number of items in the specified table. Amazon DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be reflected
      -- in this value.
    , tdKeySchema :: [KeySchemaElement]
      -- ^ The primary key structure for the table. Each KeySchemaElement consists of:
      -- AttributeName - The name of the attribute. KeyType - The key type for the
      -- attribute. Can be either HASH or RANGE. For more information about primary
      -- keys, see Primary Key in the Amazon DynamoDB Developer Guide.
    , tdLocalSecondaryIndexes :: [LocalSecondaryIndexDescription]
      -- ^ Represents one or more local secondary indexes on the table. Each index is
      -- scoped to a given hash key value. Tables with one or more local secondary
      -- indexes are subject to an item collection size limit, where the amount of
      -- data within a given item collection cannot exceed 10 GB. Each element is
      -- composed of: IndexName - The name of the local secondary index. KeySchema -
      -- Specifies the complete index key schema. The attribute names in the key
      -- schema must be between 1 and 255 characters (inclusive). The key schema
      -- must begin with the same hash key attribute as the table. Projection -
      -- Specifies attributes that are copied (projected) from the table into the
      -- index. These are in addition to the primary key attributes and index key
      -- attributes, which are automatically projected. Each attribute specification
      -- is composed of: ProjectionType - One of the following: KEYS_ONLY - Only the
      -- index and primary keys are projected into the index. INCLUDE - Only the
      -- specified table attributes are projected into the index. The list of
      -- projected attributes are in NonKeyAttributes. ALL - All of the table
      -- attributes are projected into the index. NonKeyAttributes - A list of one
      -- or more non-key attribute names that are projected into the secondary
      -- index. The total count of attributes specified in NonKeyAttributes, summed
      -- across all of the secondary indexes, must not exceed 20. If you project the
      -- same attribute into two different indexes, this counts as two distinct
      -- attributes when determining the total. IndexSizeBytes - Represents the
      -- total size of the index, in bytes. Amazon DynamoDB updates this value
      -- approximately every six hours. Recent changes might not be reflected in
      -- this value. ItemCount - Represents the number of items in the index. Amazon
      -- DynamoDB updates this value approximately every six hours. Recent changes
      -- might not be reflected in this value. If the table is in the DELETING
      -- state, no information about indexes will be returned.
    , tdProvisionedThroughput :: Maybe ProvisionedThroughputDescription
      -- ^ The provisioned throughput settings for the table, consisting of read and
      -- write capacity units, along with data about increases and decreases.
    , tdTableName :: Maybe Text
      -- ^ The name of the table.
    , tdTableSizeBytes :: Maybe Integer
      -- ^ The total size of the specified table, in bytes. Amazon DynamoDB updates
      -- this value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    , tdTableStatus :: Maybe TableStatus
      -- ^ The current state of the table: CREATING - The table is being created, as
      -- the result of a CreateTable operation. UPDATING - The table is being
      -- updated, as the result of an UpdateTable operation. DELETING - The table is
      -- being deleted, as the result of a DeleteTable operation. ACTIVE - The table
      -- is ready for use.
    } deriving (Eq, Show, Generic)

instance FromJSON TableDescription
instance ToJSON TableDescription

-- | A request to perform a PutItem operation.
newtype PutRequest = PutRequest
    { prItem :: HashMap Text AttributeValue
      -- ^ A map of attribute name to attribute values, representing the primary key
      -- of an item to be processed by PutItem. All of the table's primary key
      -- attributes must be specified, and their data types must match those of the
      -- table's key schema. If any attributes are present in the item which are
      -- part of an index key schema for the table, their types must match the index
      -- key schema.
    } deriving (Eq, Show, Generic)

instance FromJSON PutRequest
instance ToJSON PutRequest

-- | The provisioned throughput settings for the table, consisting of read and
-- write capacity units, along with data about increases and decreases.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription
    { ptdLastDecreaseDateTime :: Maybe UTCTime
      -- ^ The date and time of the last provisioned throughput decrease for this
      -- table.
    , ptdLastIncreaseDateTime :: Maybe UTCTime
      -- ^ The date and time of the last provisioned throughput increase for this
      -- table.
    , ptdNumberOfDecreasesToday :: Maybe Integer
      -- ^ The number of provisioned throughput decreases for this table during this
      -- UTC calendar day. For current maximums on provisioned throughput decreases,
      -- see Limits in the Amazon DynamoDB Developer Guide.
    , ptdReadCapacityUnits :: Maybe Integer
      -- ^ The maximum number of strongly consistent reads consumed per second before
      -- Amazon DynamoDB returns a ThrottlingException. Eventually consistent reads
      -- require less effort than strongly consistent reads, so a setting of 50
      -- ReadCapacityUnits per second provides 100 eventually consistent
      -- ReadCapacityUnits per second.
    , ptdWriteCapacityUnits :: Maybe Integer
      -- ^ The maximum number of writes consumed per second before Amazon DynamoDB
      -- returns a ThrottlingException.
    } deriving (Eq, Show, Generic)

instance FromJSON ProvisionedThroughputDescription
instance ToJSON ProvisionedThroughputDescription

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
data ProvisionedThroughput = ProvisionedThroughput
    { ptReadCapacityUnits :: !Integer
      -- ^ The maximum number of strongly consistent reads consumed per second before
      -- Amazon DynamoDB returns a ThrottlingException. For more information, see
      -- Specifying Read and Write Requirements in the Amazon DynamoDB Developer
      -- Guide.
    , ptWriteCapacityUnits :: !Integer
      -- ^ The maximum number of writes consumed per second before Amazon DynamoDB
      -- returns a ThrottlingException. For more information, see Specifying Read
      -- and Write Requirements in the Amazon DynamoDB Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromJSON ProvisionedThroughput
instance ToJSON ProvisionedThroughput

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
data Projection = Projection
    { pNonKeyAttributes :: [Text]
      -- ^ Represents the non-key attribute names which will be projected into the
      -- index. For local secondary indexes, the total count of NonKeyAttributes
      -- summed across all of the local secondary indexes, must not exceed 20. If
      -- you project the same attribute into two different indexes, this counts as
      -- two distinct attributes when determining the total.
    , pProjectionType :: Maybe ProjectionType
      -- ^ The set of attributes that are projected into the index: KEYS_ONLY - Only
      -- the index and primary keys are projected into the index. INCLUDE - Only the
      -- specified table attributes are projected into the index. The list of
      -- projected attributes are in NonKeyAttributes. ALL - All of the table
      -- attributes are projected into the index.
    } deriving (Eq, Show, Generic)

instance FromJSON Projection
instance ToJSON Projection

-- | Represents the properties of a local secondary index.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription
    { lsidIndexName :: Maybe Text
      -- ^ Represents the name of the local secondary index.
    , lsidIndexSizeBytes :: Maybe Integer
      -- ^ The total size of the specified index, in bytes. Amazon DynamoDB updates
      -- this value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    , lsidItemCount :: Maybe Integer
      -- ^ The number of items in the specified index. Amazon DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be reflected
      -- in this value.
    , lsidKeySchema :: [KeySchemaElement]
      -- ^ The complete index key schema, which consists of one or more pairs of
      -- attribute names and key types (HASH or RANGE).
    , lsidProjection :: Maybe Projection
      -- ^ Represents attributes that are copied (projected) from the table into an
      -- index. These are in addition to the primary key attributes and index key
      -- attributes, which are automatically projected.
    } deriving (Eq, Show, Generic)

instance FromJSON LocalSecondaryIndexDescription
instance ToJSON LocalSecondaryIndexDescription

-- | Represents a local secondary index.
data LocalSecondaryIndex = LocalSecondaryIndex
    { lsiIndexName :: !Text
      -- ^ The name of the local secondary index. The name must be unique among all
      -- other indexes on this table.
    , lsiKeySchema :: [KeySchemaElement]
      -- ^ The complete key schema for the local secondary index, consisting of one or
      -- more pairs of attribute names and key types (HASH or RANGE).
    , lsiProjection :: Projection
      -- ^ Represents attributes that are copied (projected) from the table into an
      -- index. These are in addition to the primary key attributes and index key
      -- attributes, which are automatically projected.
    } deriving (Eq, Show, Generic)

instance FromJSON LocalSecondaryIndex
instance ToJSON LocalSecondaryIndex

-- | Represents a set of primary keys and, for each key, the attributes to
-- retrieve from the table.
data KeysAndAttributes = KeysAndAttributes
    { kaaAttributesToGet :: [Text]
      -- ^ One or more attributes to retrieve from the table or index. If no attribute
      -- names are specified then all attributes will be returned. If any of the
      -- specified attributes are not found, they will not appear in the result. If
      -- you are querying an index and request only attributes that are projected
      -- into that index, the operation will read only the index and not the table.
      -- If any of the requested attributes are not projected into the index, Amazon
      -- DynamoDB will need to fetch each matching item from the table. This extra
      -- fetching incurs additional throughput cost and latency.
    , kaaConsistentRead :: Maybe Bool
      -- ^ The consistency of a read operation. If set to true, then a strongly
      -- consistent read is used; otherwise, an eventually consistent read is used.
    , kaaKeys :: [HashMap Text AttributeValue]
      -- ^ The primary key attribute values that define the items and the attributes
      -- associated with the items.
    } deriving (Eq, Show, Generic)

instance FromJSON KeysAndAttributes
instance ToJSON KeysAndAttributes

-- | Represents a single element of a key schema. A key schema specifies the
-- attributes that make up the primary key of a table, or the key attributes
-- of an index.
data KeySchemaElement = KeySchemaElement
    { kseAttributeName :: !Text
      -- ^ The name of a key attribute.
    , kseKeyType :: !KeyType
      -- ^ The attribute data, consisting of the data type and the attribute value
      -- itself.
    } deriving (Eq, Show, Generic)

instance FromJSON KeySchemaElement
instance ToJSON KeySchemaElement

-- | Information about item collections, if any, that were affected by the
-- operation. ItemCollectionMetrics is only returned if it was asked for in
-- the request. If the table does not have any local secondary indexes, this
-- information is not returned in the response. Each ItemCollectionMetrics
-- element consists of: ItemCollectionKey - The hash key value of the item
-- collection. This is the same as the hash key of the item. SizeEstimateRange
-- - An estimate of item collection size, measured in gigabytes. This is a
-- two-element array containing a lower bound and an upper bound for the
-- estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local secondary
-- indexes on that table. Use this estimate to measure whether a local
-- secondary index is approaching its size limit. The estimate is subject to
-- change over time; therefore, do not rely on the precision or accuracy of
-- the estimate.
data ItemCollectionMetrics = ItemCollectionMetrics
    { icmItemCollectionKey :: HashMap Text AttributeValue
      -- ^ The hash key value of the item collection. This is the same as the hash key
      -- of the item.
    , icmSizeEstimateRangeGB :: [Double]
      -- ^ An estimate of item collection size, measured in gigabytes. This is a
      -- two-element array containing a lower bound and an upper bound for the
      -- estimate. The estimate includes the size of all the items in the table,
      -- plus the size of all attributes projected into all of the local secondary
      -- indexes on that table. Use this estimate to measure whether a local
      -- secondary index is approaching its size limit. The estimate is subject to
      -- change over time; therefore, do not rely on the precision or accuracy of
      -- the estimate.
    } deriving (Eq, Show, Generic)

instance FromJSON ItemCollectionMetrics
instance ToJSON ItemCollectionMetrics

-- | Represents the new provisioned throughput settings to apply to a global
-- secondary index.
newtype GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate
    { gsiuUpdate :: Maybe UpdateGlobalSecondaryIndexAction
      -- ^ The name of a global secondary index, along with the updated provisioned
      -- throughput settings that are to be applied to that index.
    } deriving (Eq, Show, Generic)

instance FromJSON GlobalSecondaryIndexUpdate
instance ToJSON GlobalSecondaryIndexUpdate

-- | Represents the properties of a global secondary index.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription
    { gsidIndexName :: Maybe Text
      -- ^ The name of the global secondary index.
    , gsidIndexSizeBytes :: Maybe Integer
      -- ^ The total size of the specified index, in bytes. Amazon DynamoDB updates
      -- this value approximately every six hours. Recent changes might not be
      -- reflected in this value.
    , gsidIndexStatus :: Maybe IndexStatus
      -- ^ The current state of the global secondary index: CREATING - The index is
      -- being created, as the result of a CreateTable or UpdateTable operation.
      -- UPDATING - The index is being updated, as the result of a CreateTable or
      -- UpdateTable operation. DELETING - The index is being deleted, as the result
      -- of a DeleteTable operation. ACTIVE - The index is ready for use.
    , gsidItemCount :: Maybe Integer
      -- ^ The number of items in the specified index. Amazon DynamoDB updates this
      -- value approximately every six hours. Recent changes might not be reflected
      -- in this value.
    , gsidKeySchema :: [KeySchemaElement]
      -- ^ The complete key schema for the global secondary index, consisting of one
      -- or more pairs of attribute names and key types (HASH or RANGE).
    , gsidProjection :: Maybe Projection
      -- ^ Represents attributes that are copied (projected) from the table into an
      -- index. These are in addition to the primary key attributes and index key
      -- attributes, which are automatically projected.
    , gsidProvisionedThroughput :: Maybe ProvisionedThroughputDescription
      -- ^ Represents the provisioned throughput settings for the table, consisting of
      -- read and write capacity units, along with data about increases and
      -- decreases.
    } deriving (Eq, Show, Generic)

instance FromJSON GlobalSecondaryIndexDescription
instance ToJSON GlobalSecondaryIndexDescription

-- | Represents a global secondary index.
data GlobalSecondaryIndex = GlobalSecondaryIndex
    { gsiIndexName :: !Text
      -- ^ The name of the global secondary index. The name must be unique among all
      -- other indexes on this table.
    , gsiKeySchema :: [KeySchemaElement]
      -- ^ The complete key schema for a global secondary index, which consists of one
      -- or more pairs of attribute names and key types (HASH or RANGE).
    , gsiProjection :: Projection
      -- ^ Represents attributes that are copied (projected) from the table into an
      -- index. These are in addition to the primary key attributes and index key
      -- attributes, which are automatically projected.
    , gsiProvisionedThroughput :: ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified table or
      -- index. The settings can be modified using the UpdateTable operation. For
      -- current minimum and maximum provisioned throughput values, see Limits in
      -- the Amazon DynamoDB Developer Guide.
    } deriving (Eq, Show, Generic)

instance FromJSON GlobalSecondaryIndex
instance ToJSON GlobalSecondaryIndex

-- | Represents an attribute value used with conditional DeleteItem, PutItem or
-- UpdateItem operations. Amazon DynamoDB will check to see if the attribute
-- value already exists; or if the attribute exists and has a particular value
-- before updating it.
data ExpectedAttributeValue = ExpectedAttributeValue
    { eavExists :: Maybe Bool
      -- ^ Causes Amazon DynamoDB to evaluate the value before attempting a
      -- conditional operation: If Exists is true, Amazon DynamoDB will check to see
      -- if that attribute value already exists in the table. If it is found, then
      -- the operation succeeds. If it is not found, the operation fails with a
      -- ConditionalCheckFailedException. If Exists is false, Amazon DynamoDB
      -- assumes that the attribute value does not exist in the table. If in fact
      -- the value does not exist, then the assumption is valid and the operation
      -- succeeds. If the value is found, despite the assumption that it does not
      -- exist, the operation fails with a ConditionalCheckFailedException. The
      -- default setting for Exists is true. If you supply a Value all by itself,
      -- Amazon DynamoDB assumes the attribute exists: You don't have to set Exists
      -- to true, because it is implied. Amazon DynamoDB returns a
      -- ValidationException if: Exists is true but there is no Value to check. (You
      -- expect a value to exist, but don't specify what that value is.) Exists is
      -- false but you also specify a Value. (You cannot expect an attribute to have
      -- a value, while also expecting it not to exist.) If you specify more than
      -- one condition for Exists, then all of the conditions must evaluate to true.
      -- (In other words, the conditions are ANDed together.) Otherwise, the
      -- conditional operation will fail.
    , eavValue :: Maybe AttributeValue
      -- ^ Represents the data for an attribute. You can set one, and only one, of the
      -- elements.
    } deriving (Eq, Show, Generic)

instance FromJSON ExpectedAttributeValue
instance ToJSON ExpectedAttributeValue

-- | A request to perform a DeleteItem operation.
newtype DeleteRequest = DeleteRequest
    { drKey :: HashMap Text AttributeValue
      -- ^ A map of attribute name to attribute values, representing the primary key
      -- of the item to delete. All of the table's primary key attributes must be
      -- specified, and their data types must match those of the table's key schema.
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteRequest
instance ToJSON DeleteRequest

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
data ConsumedCapacity = ConsumedCapacity
    { ccCapacityUnits :: Maybe Double
      -- ^ The total number of capacity units consumed by the operation.
    , ccGlobalSecondaryIndexes :: HashMap Text Capacity
      -- ^ The amount of throughput consumed on each global index affected by the
      -- operation.
    , ccLocalSecondaryIndexes :: HashMap Text Capacity
      -- ^ The amount of throughput consumed on each local index affected by the
      -- operation.
    , ccTable :: Maybe Capacity
      -- ^ The amount of throughput consumed on the table affected by the operation.
    , ccTableName :: Maybe Text
      -- ^ The name of the table that was affected by the operation.
    } deriving (Eq, Show, Generic)

instance FromJSON ConsumedCapacity
instance ToJSON ConsumedCapacity

-- | Represents a selection criteria for a Query or Scan operation. For a Query
-- operation, the condition specifies the key attributes to use when querying
-- a table or an index. For a Scan operation, the condition is used to
-- evaluate the scan results and return only the desired values. Multiple
-- conditions are "ANDed" together. In other words, all of the conditions must
-- be met to be included in the output.
data Condition = Condition
    { dAttributeValueList :: [AttributeValue]
      -- ^ One or more values to evaluate against the supplied attribute. This list
      -- contains exactly one value, except for a BETWEEN or IN comparison, in which
      -- case the list contains two values. For type Number, value comparisons are
      -- numeric. String value comparisons for greater than, equals, or less than
      -- are based on ASCII character code values. For example, a is greater than A,
      -- and aa is greater than B. For a list of code values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For Binary,
      -- Amazon DynamoDB treats each byte of the binary data as unsigned when it
      -- compares binary values, for example when evaluating query expressions.
    , dComparisonOperator :: !ComparisonOperator
      -- ^ A comparator for evaluating attributes. For example, equals, greater than,
      -- less than, etc. Valid comparison operators for Query: EQ | LE | LT | GE |
      -- GT | BEGINS_WITH | BETWEEN Valid comparison operators for Scan: EQ | NE |
      -- LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH
      -- | IN | BETWEEN For information on specifying data types in JSON, see JSON
      -- Data Format in the Amazon DynamoDB Developer Guide. The following are
      -- descriptions of each comparison operator. EQ : Equal. AttributeValueList
      -- can contain only one AttributeValue of type String, Number, or Binary (not
      -- a set). If an item contains an AttributeValue of a different type than the
      -- one specified in the request, the value does not match. For example,
      -- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not equal
      -- {"NS":["6", "2", "1"]}. NE : Not equal. AttributeValueList can contain only
      -- one AttributeValue of type String, Number, or Binary (not a set). If an
      -- item contains an AttributeValue of a different type than the one specified
      -- in the request, the value does not match. For example, {"S":"6"} does not
      -- equal {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE
      -- : Less than or equal. AttributeValueList can contain only one
      -- AttributeValue of type String, Number, or Binary (not a set). If an item
      -- contains an AttributeValue of a different type than the one specified in
      -- the request, the value does not match. For example, {"S":"6"} does not
      -- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
      -- "1"]}. LT : Less than. AttributeValueList can contain only one
      -- AttributeValue of type String, Number, or Binary (not a set). If an item
      -- contains an AttributeValue of a different type than the one specified in
      -- the request, the value does not match. For example, {"S":"6"} does not
      -- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
      -- "1"]}. GE : Greater than or equal. AttributeValueList can contain only one
      -- AttributeValue of type String, Number, or Binary (not a set). If an item
      -- contains an AttributeValue of a different type than the one specified in
      -- the request, the value does not match. For example, {"S":"6"} does not
      -- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
      -- "1"]}. GT : Greater than. AttributeValueList can contain only one
      -- AttributeValue of type String, Number, or Binary (not a set). If an item
      -- contains an AttributeValue of a different type than the one specified in
      -- the request, the value does not match. For example, {"S":"6"} does not
      -- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
      -- "1"]}. NOT_NULL : The attribute exists. NULL : The attribute does not
      -- exist. CONTAINS : checks for a subsequence, or value in a set.
      -- AttributeValueList can contain only one AttributeValue of type String,
      -- Number, or Binary (not a set). If the target attribute of the comparison is
      -- a String, then the operation checks for a substring match. If the target
      -- attribute of the comparison is Binary, then the operation looks for a
      -- subsequence of the target that matches the input. If the target attribute
      -- of the comparison is a set ("SS", "NS", or "BS"), then the operation checks
      -- for a member of the set (not as a substring). NOT_CONTAINS : checks for
      -- absence of a subsequence, or absence of a value in a set.
      -- AttributeValueList can contain only one AttributeValue of type String,
      -- Number, or Binary (not a set). If the target attribute of the comparison is
      -- a String, then the operation checks for the absence of a substring match.
      -- If the target attribute of the comparison is Binary, then the operation
      -- checks for the absence of a subsequence of the target that matches the
      -- input. If the target attribute of the comparison is a set ("SS", "NS", or
      -- "BS"), then the operation checks for the absence of a member of the set
      -- (not as a substring). BEGINS_WITH : checks for a prefix. AttributeValueList
      -- can contain only one AttributeValue of type String or Binary (not a Number
      -- or a set). The target attribute of the comparison must be a String or
      -- Binary (not a Number or a set). IN : checks for exact matches.
      -- AttributeValueList can contain more than one AttributeValue of type String,
      -- Number, or Binary (not a set). The target attribute of the comparison must
      -- be of the same type and exact value to match. A String never matches a
      -- String set. BETWEEN : Greater than or equal to the first value, and less
      -- than or equal to the second value. AttributeValueList must contain two
      -- AttributeValue elements of the same type, either String, Number, or Binary
      -- (not a set). A target attribute matches if the target value is greater
      -- than, or equal to, the first element and less than, or equal to, the second
      -- element. If an item contains an AttributeValue of a different type than the
      -- one specified in the request, the value does not match. For example,
      -- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not compare
      -- to {"NS":["6", "2", "1"]}.
    } deriving (Eq, Show, Generic)

instance FromJSON Condition
instance ToJSON Condition

-- | Represents the amount of provisioned throughput capacity consumed on a
-- table or an index.
newtype Capacity = Capacity
    { cCapacityUnits :: Maybe Double
      -- ^ The total number of capacity units consumed on a table or an index.
    } deriving (Eq, Show, Generic)

instance FromJSON Capacity
instance ToJSON Capacity

-- | For the UpdateItem operation, represents the attributes to be modified,the
-- action to perform on each, and the new value for each. You cannot use
-- UpdateItem to update any primary key attributes. Instead, you will need to
-- delete the item, and then use PutItem to create a new item with new
-- attributes. Attribute values cannot be null; string and binary type
-- attributes must have lengths greater than zero; and set type attributes
-- must not be empty. Requests with empty values will be rejected with a
-- ValidationException.
data AttributeValueUpdate = AttributeValueUpdate
    { avuAction :: Maybe AttributeAction
      -- ^ Specifies how to perform the update. Valid values are PUT, DELETE, and ADD.
      -- The behavior depends on whether the specified primary key already exists in
      -- the table. If an item with the specified Key is found in the table: PUT -
      -- Adds the specified attribute to the item. If the attribute already exists,
      -- it is replaced by the new value. DELETE - If no value is specified, the
      -- attribute and its value are removed from the item. The data type of the
      -- specified value must match the existing value's data type. If a set of
      -- values is specified, then those values are subtracted from the old set. For
      -- example, if the attribute value was the set [a,b,c] and the DELETE action
      -- specified [a,c], then the final attribute value would be [b]. Specifying an
      -- empty set is an error. ADD - If the attribute does not already exist, then
      -- the attribute and its values are added to the item. If the attribute does
      -- exist, then the behavior of ADD depends on the data type of the attribute:
      -- If the existing attribute is a number, and if Value is also a number, then
      -- the Value is mathematically added to the existing attribute. If Value is a
      -- negative number, then it is subtracted from the existing attribute. If you
      -- use ADD to increment or decrement a number value for an item that doesn't
      -- exist before the update, Amazon DynamoDB uses 0 as the initial value. In
      -- addition, if you use ADD to update an existing item, and intend to
      -- increment or decrement an attribute value which does not yet exist, Amazon
      -- DynamoDB uses 0 as the initial value. For example, suppose that the item
      -- you want to update does not yet have an attribute named itemcount, but you
      -- decide to ADD the number 3 to this attribute anyway, even though it
      -- currently does not exist. Amazon DynamoDB will create the itemcount
      -- attribute, set its initial value to 0, and finally add 3 to it. The result
      -- will be a new itemcount attribute in the item, with a value of 3. If the
      -- existing data type is a set, and if the Value is also a set, then the Value
      -- is added to the existing set. (This is a set operation, not mathematical
      -- addition.) For example, if the attribute value was the set [1,2], and the
      -- ADD action specified [3], then the final attribute value would be [1,2,3].
      -- An error occurs if an Add action is specified for a set attribute and the
      -- attribute type specified does not match the existing set type. Both sets
      -- must have the same primitive data type. For example, if the existing data
      -- type is a set of strings, the Value must also be a set of strings. The same
      -- holds true for number sets and binary sets. This action is only valid for
      -- an existing attribute whose data type is number or is a set. Do not use ADD
      -- for any other data types. If no item with the specified Key is found: PUT -
      -- Amazon DynamoDB creates a new item with the specified primary key, and then
      -- adds the attribute. DELETE - Nothing happens; there is no attribute to
      -- delete. ADD - Amazon DynamoDB creates an item with the supplied primary key
      -- and number (or set of numbers) for the attribute value. The only data types
      -- allowed are number and number set; no other data types can be specified.
    , avuValue :: Maybe AttributeValue
      -- ^ Represents the data for an attribute. You can set one, and only one, of the
      -- elements.
    } deriving (Eq, Show, Generic)

instance FromJSON AttributeValueUpdate
instance ToJSON AttributeValueUpdate

-- | Represents the data for an attribute. You can set one, and only one, of the
-- elements.
data AttributeValue = AttributeValue
    { avB :: Maybe Blob
      -- ^ A Binary data type.
    , avBS :: [Blob]
      -- ^ A Binary set data type.
    , avN :: Maybe Text
      -- ^ A Number data type.
    , avNS :: [Text]
      -- ^ Number set data type.
    , avS :: Maybe Text
      -- ^ A String data type.
    , avSS :: [Text]
      -- ^ A String set data type.
    } deriving (Eq, Show, Generic)

instance FromJSON AttributeValue
instance ToJSON AttributeValue

-- | Represents an attribute for describing the key schema for the table and
-- indexes.
data AttributeDefinition = AttributeDefinition
    { adAttributeName :: !Text
      -- ^ A name for the attribute.
    , adAttributeType :: !ScalarAttributeType
      -- ^ The data type for the attribute.
    } deriving (Eq, Show, Generic)

instance FromJSON AttributeDefinition
instance ToJSON AttributeDefinition

-- | The current state of the table: CREATING - The table is being created, as
-- the result of a CreateTable operation. UPDATING - The table is being
-- updated, as the result of an UpdateTable operation. DELETING - The table is
-- being deleted, as the result of a DeleteTable operation. ACTIVE - The table
-- is ready for use.

data TableStatus
    = TableStatusACTIVE
    | TableStatusCREATING
    | TableStatusDELETING
    | TableStatusUPDATING
      deriving (Eq, Ord, Generic)

instance Hashable TableStatus

instance FromText TableStatus where
    fromText "ACTIVE" = Right TableStatusACTIVE
    fromText "CREATING" = Right TableStatusCREATING
    fromText "DELETING" = Right TableStatusDELETING
    fromText "UPDATING" = Right TableStatusUPDATING
    fromText e = fromTextFail $ "Unrecognised TableStatus: " <> e

instance Read TableStatus where
    readsPrec _ = fromTextRead

instance ToText TableStatus where
    toText TableStatusACTIVE = "ACTIVE"
    toText TableStatusCREATING = "CREATING"
    toText TableStatusDELETING = "DELETING"
    toText TableStatusUPDATING = "UPDATING"

instance Show TableStatus where
    show = toTextShow

instance FromJSON TableStatus where
    parseJSON = fromTextJSON "TableStatus"

instance FromJSON v => FromJSON (HashMap TableStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON TableStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap TableStatus v) where
    toJSON = toTextHashJSON

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, or the count of matching items.
-- ALL_ATTRIBUTES: Returns all of the item attributes. COUNT: Returns the
-- number of matching items, rather than the matching items themselves.
-- SPECIFIC_ATTRIBUTES : Returns only the attributes listed in
-- AttributesToGet. This is equivalent to specifying AttributesToGet without
-- specifying any value for Select. If neither Select nor AttributesToGet are
-- specified, Amazon DynamoDB defaults to ALL_ATTRIBUTES. You cannot use both
-- Select and AttributesToGet together in a single request, unless the value
-- for Select is SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
-- AttributesToGet without any value for Select.).

data Select
    = SelectALL_ATTRIBUTES
    | SelectALL_PROJECTED_ATTRIBUTES
    | SelectCOUNT
    | SelectSPECIFIC_ATTRIBUTES
      deriving (Eq, Ord, Generic)

instance Hashable Select

instance FromText Select where
    fromText "ALL_ATTRIBUTES" = Right SelectALL_ATTRIBUTES
    fromText "ALL_PROJECTED_ATTRIBUTES" = Right SelectALL_PROJECTED_ATTRIBUTES
    fromText "COUNT" = Right SelectCOUNT
    fromText "SPECIFIC_ATTRIBUTES" = Right SelectSPECIFIC_ATTRIBUTES
    fromText e = fromTextFail $ "Unrecognised Select: " <> e

instance Read Select where
    readsPrec _ = fromTextRead

instance ToText Select where
    toText SelectALL_ATTRIBUTES = "ALL_ATTRIBUTES"
    toText SelectALL_PROJECTED_ATTRIBUTES = "ALL_PROJECTED_ATTRIBUTES"
    toText SelectCOUNT = "COUNT"
    toText SelectSPECIFIC_ATTRIBUTES = "SPECIFIC_ATTRIBUTES"

instance Show Select where
    show = toTextShow

instance FromJSON Select where
    parseJSON = fromTextJSON "Select"

instance FromJSON v => FromJSON (HashMap Select v) where
    parseJSON = fromTextHashJSON

instance ToJSON Select where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap Select v) where
    toJSON = toTextHashJSON

-- | The data type for the attribute.

data ScalarAttributeType
    = ScalarAttributeTypeB
    | ScalarAttributeTypeN
    | ScalarAttributeTypeS
      deriving (Eq, Ord, Generic)

instance Hashable ScalarAttributeType

instance FromText ScalarAttributeType where
    fromText "B" = Right ScalarAttributeTypeB
    fromText "N" = Right ScalarAttributeTypeN
    fromText "S" = Right ScalarAttributeTypeS
    fromText e = fromTextFail $ "Unrecognised ScalarAttributeType: " <> e

instance Read ScalarAttributeType where
    readsPrec _ = fromTextRead

instance ToText ScalarAttributeType where
    toText ScalarAttributeTypeB = "B"
    toText ScalarAttributeTypeN = "N"
    toText ScalarAttributeTypeS = "S"

instance Show ScalarAttributeType where
    show = toTextShow

instance FromJSON ScalarAttributeType where
    parseJSON = fromTextJSON "ScalarAttributeType"

instance FromJSON v => FromJSON (HashMap ScalarAttributeType v) where
    parseJSON = fromTextHashJSON

instance ToJSON ScalarAttributeType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ScalarAttributeType v) where
    toJSON = toTextHashJSON

-- | Use ReturnValues if you want to get the item attributes as they appeared
-- before they were updated with the PutItem request. For PutItem, the valid
-- values are: NONE - If ReturnValues is not specified, or if its value is
-- NONE, then nothing is returned. (This is the default for ReturnValues.)
-- ALL_OLD - If PutItem overwrote an attribute name-value pair, then the
-- content of the old item is returned.

data ReturnValue
    = ReturnValueALL_NEW
    | ReturnValueALL_OLD
    | ReturnValueNONE
    | ReturnValueUPDATED_NEW
    | ReturnValueUPDATED_OLD
      deriving (Eq, Ord, Generic)

instance Hashable ReturnValue

instance FromText ReturnValue where
    fromText "ALL_NEW" = Right ReturnValueALL_NEW
    fromText "ALL_OLD" = Right ReturnValueALL_OLD
    fromText "NONE" = Right ReturnValueNONE
    fromText "UPDATED_NEW" = Right ReturnValueUPDATED_NEW
    fromText "UPDATED_OLD" = Right ReturnValueUPDATED_OLD
    fromText e = fromTextFail $ "Unrecognised ReturnValue: " <> e

instance Read ReturnValue where
    readsPrec _ = fromTextRead

instance ToText ReturnValue where
    toText ReturnValueALL_NEW = "ALL_NEW"
    toText ReturnValueALL_OLD = "ALL_OLD"
    toText ReturnValueNONE = "NONE"
    toText ReturnValueUPDATED_NEW = "UPDATED_NEW"
    toText ReturnValueUPDATED_OLD = "UPDATED_OLD"

instance Show ReturnValue where
    show = toTextShow

instance FromJSON ReturnValue where
    parseJSON = fromTextJSON "ReturnValue"

instance FromJSON v => FromJSON (HashMap ReturnValue v) where
    parseJSON = fromTextHashJSON

instance ToJSON ReturnValue where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ReturnValue v) where
    toJSON = toTextHashJSON

-- | If set to SIZE, statistics about item collections, if any, that were
-- modified during the operation are returned in the response. If set to NONE
-- (the default), no statistics are returned.

data ReturnItemCollectionMetrics
    = ReturnItemCollectionMetricsNONE
    | ReturnItemCollectionMetricsSIZE
      deriving (Eq, Ord, Generic)

instance Hashable ReturnItemCollectionMetrics

instance FromText ReturnItemCollectionMetrics where
    fromText "NONE" = Right ReturnItemCollectionMetricsNONE
    fromText "SIZE" = Right ReturnItemCollectionMetricsSIZE
    fromText e = fromTextFail $ "Unrecognised ReturnItemCollectionMetrics: " <> e

instance Read ReturnItemCollectionMetrics where
    readsPrec _ = fromTextRead

instance ToText ReturnItemCollectionMetrics where
    toText ReturnItemCollectionMetricsNONE = "NONE"
    toText ReturnItemCollectionMetricsSIZE = "SIZE"

instance Show ReturnItemCollectionMetrics where
    show = toTextShow

instance FromJSON ReturnItemCollectionMetrics where
    parseJSON = fromTextJSON "ReturnItemCollectionMetrics"

instance FromJSON v => FromJSON (HashMap ReturnItemCollectionMetrics v) where
    parseJSON = fromTextHashJSON

instance ToJSON ReturnItemCollectionMetrics where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ReturnItemCollectionMetrics v) where
    toJSON = toTextHashJSON

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.

data ReturnConsumedCapacity
    = ReturnConsumedCapacityINDEXES
    | ReturnConsumedCapacityNONE
    | ReturnConsumedCapacityTOTAL
      deriving (Eq, Ord, Generic)

instance Hashable ReturnConsumedCapacity

instance FromText ReturnConsumedCapacity where
    fromText "INDEXES" = Right ReturnConsumedCapacityINDEXES
    fromText "NONE" = Right ReturnConsumedCapacityNONE
    fromText "TOTAL" = Right ReturnConsumedCapacityTOTAL
    fromText e = fromTextFail $ "Unrecognised ReturnConsumedCapacity: " <> e

instance Read ReturnConsumedCapacity where
    readsPrec _ = fromTextRead

instance ToText ReturnConsumedCapacity where
    toText ReturnConsumedCapacityINDEXES = "INDEXES"
    toText ReturnConsumedCapacityNONE = "NONE"
    toText ReturnConsumedCapacityTOTAL = "TOTAL"

instance Show ReturnConsumedCapacity where
    show = toTextShow

instance FromJSON ReturnConsumedCapacity where
    parseJSON = fromTextJSON "ReturnConsumedCapacity"

instance FromJSON v => FromJSON (HashMap ReturnConsumedCapacity v) where
    parseJSON = fromTextHashJSON

instance ToJSON ReturnConsumedCapacity where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ReturnConsumedCapacity v) where
    toJSON = toTextHashJSON

-- | The set of attributes that are projected into the index: KEYS_ONLY - Only
-- the index and primary keys are projected into the index. INCLUDE - Only the
-- specified table attributes are projected into the index. The list of
-- projected attributes are in NonKeyAttributes. ALL - All of the table
-- attributes are projected into the index.

data ProjectionType
    = ProjectionTypeALL
    | ProjectionTypeINCLUDE
    | ProjectionTypeKEYS_ONLY
      deriving (Eq, Ord, Generic)

instance Hashable ProjectionType

instance FromText ProjectionType where
    fromText "ALL" = Right ProjectionTypeALL
    fromText "INCLUDE" = Right ProjectionTypeINCLUDE
    fromText "KEYS_ONLY" = Right ProjectionTypeKEYS_ONLY
    fromText e = fromTextFail $ "Unrecognised ProjectionType: " <> e

instance Read ProjectionType where
    readsPrec _ = fromTextRead

instance ToText ProjectionType where
    toText ProjectionTypeALL = "ALL"
    toText ProjectionTypeINCLUDE = "INCLUDE"
    toText ProjectionTypeKEYS_ONLY = "KEYS_ONLY"

instance Show ProjectionType where
    show = toTextShow

instance FromJSON ProjectionType where
    parseJSON = fromTextJSON "ProjectionType"

instance FromJSON v => FromJSON (HashMap ProjectionType v) where
    parseJSON = fromTextHashJSON

instance ToJSON ProjectionType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ProjectionType v) where
    toJSON = toTextHashJSON

-- | The attribute data, consisting of the data type and the attribute value
-- itself.

data KeyType
    = KeyTypeHASH
    | KeyTypeRANGE
      deriving (Eq, Ord, Generic)

instance Hashable KeyType

instance FromText KeyType where
    fromText "HASH" = Right KeyTypeHASH
    fromText "RANGE" = Right KeyTypeRANGE
    fromText e = fromTextFail $ "Unrecognised KeyType: " <> e

instance Read KeyType where
    readsPrec _ = fromTextRead

instance ToText KeyType where
    toText KeyTypeHASH = "HASH"
    toText KeyTypeRANGE = "RANGE"

instance Show KeyType where
    show = toTextShow

instance FromJSON KeyType where
    parseJSON = fromTextJSON "KeyType"

instance FromJSON v => FromJSON (HashMap KeyType v) where
    parseJSON = fromTextHashJSON

instance ToJSON KeyType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap KeyType v) where
    toJSON = toTextHashJSON

-- | The current state of the global secondary index: CREATING - The index is
-- being created, as the result of a CreateTable or UpdateTable operation.
-- UPDATING - The index is being updated, as the result of a CreateTable or
-- UpdateTable operation. DELETING - The index is being deleted, as the result
-- of a DeleteTable operation. ACTIVE - The index is ready for use.

data IndexStatus
    = IndexStatusACTIVE
    | IndexStatusCREATING
    | IndexStatusDELETING
    | IndexStatusUPDATING
      deriving (Eq, Ord, Generic)

instance Hashable IndexStatus

instance FromText IndexStatus where
    fromText "ACTIVE" = Right IndexStatusACTIVE
    fromText "CREATING" = Right IndexStatusCREATING
    fromText "DELETING" = Right IndexStatusDELETING
    fromText "UPDATING" = Right IndexStatusUPDATING
    fromText e = fromTextFail $ "Unrecognised IndexStatus: " <> e

instance Read IndexStatus where
    readsPrec _ = fromTextRead

instance ToText IndexStatus where
    toText IndexStatusACTIVE = "ACTIVE"
    toText IndexStatusCREATING = "CREATING"
    toText IndexStatusDELETING = "DELETING"
    toText IndexStatusUPDATING = "UPDATING"

instance Show IndexStatus where
    show = toTextShow

instance FromJSON IndexStatus where
    parseJSON = fromTextJSON "IndexStatus"

instance FromJSON v => FromJSON (HashMap IndexStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON IndexStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap IndexStatus v) where
    toJSON = toTextHashJSON

-- | A comparator for evaluating attributes. For example, equals, greater than,
-- less than, etc. Valid comparison operators for Query: EQ | LE | LT | GE |
-- GT | BEGINS_WITH | BETWEEN Valid comparison operators for Scan: EQ | NE |
-- LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH
-- | IN | BETWEEN For information on specifying data types in JSON, see JSON
-- Data Format in the Amazon DynamoDB Developer Guide. The following are
-- descriptions of each comparison operator. EQ : Equal. AttributeValueList
-- can contain only one AttributeValue of type String, Number, or Binary (not
-- a set). If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not equal
-- {"NS":["6", "2", "1"]}. NE : Not equal. AttributeValueList can contain only
-- one AttributeValue of type String, Number, or Binary (not a set). If an
-- item contains an AttributeValue of a different type than the one specified
-- in the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE
-- : Less than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. LT : Less than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GE : Greater than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GT : Greater than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. NOT_NULL : The attribute exists. NULL : The attribute does not
-- exist. CONTAINS : checks for a subsequence, or value in a set.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set). If the target attribute of the comparison is
-- a String, then the operation checks for a substring match. If the target
-- attribute of the comparison is Binary, then the operation looks for a
-- subsequence of the target that matches the input. If the target attribute
-- of the comparison is a set ("SS", "NS", or "BS"), then the operation checks
-- for a member of the set (not as a substring). NOT_CONTAINS : checks for
-- absence of a subsequence, or absence of a value in a set.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set). If the target attribute of the comparison is
-- a String, then the operation checks for the absence of a substring match.
-- If the target attribute of the comparison is Binary, then the operation
-- checks for the absence of a subsequence of the target that matches the
-- input. If the target attribute of the comparison is a set ("SS", "NS", or
-- "BS"), then the operation checks for the absence of a member of the set
-- (not as a substring). BEGINS_WITH : checks for a prefix. AttributeValueList
-- can contain only one AttributeValue of type String or Binary (not a Number
-- or a set). The target attribute of the comparison must be a String or
-- Binary (not a Number or a set). IN : checks for exact matches.
-- AttributeValueList can contain more than one AttributeValue of type String,
-- Number, or Binary (not a set). The target attribute of the comparison must
-- be of the same type and exact value to match. A String never matches a
-- String set. BETWEEN : Greater than or equal to the first value, and less
-- than or equal to the second value. AttributeValueList must contain two
-- AttributeValue elements of the same type, either String, Number, or Binary
-- (not a set). A target attribute matches if the target value is greater
-- than, or equal to, the first element and less than, or equal to, the second
-- element. If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not compare
-- to {"NS":["6", "2", "1"]}.

data ComparisonOperator
    = ComparisonOperatorBEGINS_WITH
    | ComparisonOperatorBETWEEN
    | ComparisonOperatorCONTAINS
    | ComparisonOperatorEQ
    | ComparisonOperatorGE
    | ComparisonOperatorGT
    | ComparisonOperatorIN
    | ComparisonOperatorLE
    | ComparisonOperatorLT
    | ComparisonOperatorNE
    | ComparisonOperatorNOT_CONTAINS
    | ComparisonOperatorNOT_NULL
    | ComparisonOperatorNULL
      deriving (Eq, Ord, Generic)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    fromText "BEGINS_WITH" = Right ComparisonOperatorBEGINS_WITH
    fromText "BETWEEN" = Right ComparisonOperatorBETWEEN
    fromText "CONTAINS" = Right ComparisonOperatorCONTAINS
    fromText "EQ" = Right ComparisonOperatorEQ
    fromText "GE" = Right ComparisonOperatorGE
    fromText "GT" = Right ComparisonOperatorGT
    fromText "IN" = Right ComparisonOperatorIN
    fromText "LE" = Right ComparisonOperatorLE
    fromText "LT" = Right ComparisonOperatorLT
    fromText "NE" = Right ComparisonOperatorNE
    fromText "NOT_CONTAINS" = Right ComparisonOperatorNOT_CONTAINS
    fromText "NOT_NULL" = Right ComparisonOperatorNOT_NULL
    fromText "NULL" = Right ComparisonOperatorNULL
    fromText e = fromTextFail $ "Unrecognised ComparisonOperator: " <> e

instance Read ComparisonOperator where
    readsPrec _ = fromTextRead

instance ToText ComparisonOperator where
    toText ComparisonOperatorBEGINS_WITH = "BEGINS_WITH"
    toText ComparisonOperatorBETWEEN = "BETWEEN"
    toText ComparisonOperatorCONTAINS = "CONTAINS"
    toText ComparisonOperatorEQ = "EQ"
    toText ComparisonOperatorGE = "GE"
    toText ComparisonOperatorGT = "GT"
    toText ComparisonOperatorIN = "IN"
    toText ComparisonOperatorLE = "LE"
    toText ComparisonOperatorLT = "LT"
    toText ComparisonOperatorNE = "NE"
    toText ComparisonOperatorNOT_CONTAINS = "NOT_CONTAINS"
    toText ComparisonOperatorNOT_NULL = "NOT_NULL"
    toText ComparisonOperatorNULL = "NULL"

instance Show ComparisonOperator where
    show = toTextShow

instance FromJSON ComparisonOperator where
    parseJSON = fromTextJSON "ComparisonOperator"

instance FromJSON v => FromJSON (HashMap ComparisonOperator v) where
    parseJSON = fromTextHashJSON

instance ToJSON ComparisonOperator where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ComparisonOperator v) where
    toJSON = toTextHashJSON

-- | Specifies how to perform the update. Valid values are PUT, DELETE, and ADD.
-- The behavior depends on whether the specified primary key already exists in
-- the table. If an item with the specified Key is found in the table: PUT -
-- Adds the specified attribute to the item. If the attribute already exists,
-- it is replaced by the new value. DELETE - If no value is specified, the
-- attribute and its value are removed from the item. The data type of the
-- specified value must match the existing value's data type. If a set of
-- values is specified, then those values are subtracted from the old set. For
-- example, if the attribute value was the set [a,b,c] and the DELETE action
-- specified [a,c], then the final attribute value would be [b]. Specifying an
-- empty set is an error. ADD - If the attribute does not already exist, then
-- the attribute and its values are added to the item. If the attribute does
-- exist, then the behavior of ADD depends on the data type of the attribute:
-- If the existing attribute is a number, and if Value is also a number, then
-- the Value is mathematically added to the existing attribute. If Value is a
-- negative number, then it is subtracted from the existing attribute. If you
-- use ADD to increment or decrement a number value for an item that doesn't
-- exist before the update, Amazon DynamoDB uses 0 as the initial value. In
-- addition, if you use ADD to update an existing item, and intend to
-- increment or decrement an attribute value which does not yet exist, Amazon
-- DynamoDB uses 0 as the initial value. For example, suppose that the item
-- you want to update does not yet have an attribute named itemcount, but you
-- decide to ADD the number 3 to this attribute anyway, even though it
-- currently does not exist. Amazon DynamoDB will create the itemcount
-- attribute, set its initial value to 0, and finally add 3 to it. The result
-- will be a new itemcount attribute in the item, with a value of 3. If the
-- existing data type is a set, and if the Value is also a set, then the Value
-- is added to the existing set. (This is a set operation, not mathematical
-- addition.) For example, if the attribute value was the set [1,2], and the
-- ADD action specified [3], then the final attribute value would be [1,2,3].
-- An error occurs if an Add action is specified for a set attribute and the
-- attribute type specified does not match the existing set type. Both sets
-- must have the same primitive data type. For example, if the existing data
-- type is a set of strings, the Value must also be a set of strings. The same
-- holds true for number sets and binary sets. This action is only valid for
-- an existing attribute whose data type is number or is a set. Do not use ADD
-- for any other data types. If no item with the specified Key is found: PUT -
-- Amazon DynamoDB creates a new item with the specified primary key, and then
-- adds the attribute. DELETE - Nothing happens; there is no attribute to
-- delete. ADD - Amazon DynamoDB creates an item with the supplied primary key
-- and number (or set of numbers) for the attribute value. The only data types
-- allowed are number and number set; no other data types can be specified.

data AttributeAction
    = AttributeActionADD
    | AttributeActionDELETE
    | AttributeActionPUT
      deriving (Eq, Ord, Generic)

instance Hashable AttributeAction

instance FromText AttributeAction where
    fromText "ADD" = Right AttributeActionADD
    fromText "DELETE" = Right AttributeActionDELETE
    fromText "PUT" = Right AttributeActionPUT
    fromText e = fromTextFail $ "Unrecognised AttributeAction: " <> e

instance Read AttributeAction where
    readsPrec _ = fromTextRead

instance ToText AttributeAction where
    toText AttributeActionADD = "ADD"
    toText AttributeActionDELETE = "DELETE"
    toText AttributeActionPUT = "PUT"

instance Show AttributeAction where
    show = toTextShow

instance FromJSON AttributeAction where
    parseJSON = fromTextJSON "AttributeAction"

instance FromJSON v => FromJSON (HashMap AttributeAction v) where
    parseJSON = fromTextHashJSON

instance ToJSON AttributeAction where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap AttributeAction v) where
    toJSON = toTextHashJSON
