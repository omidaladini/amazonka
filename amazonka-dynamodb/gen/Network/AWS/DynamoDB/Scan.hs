{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.Scan
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Scan operation returns one or more items and item attributes by
-- accessing every item in the table. To have DynamoDB return fewer items, you
-- can provide a ScanFilter. If the total number of scanned items exceeds the
-- maximum data set size limit of 1 MB, the scan stops and results are
-- returned to the user with a LastEvaluatedKey to continue the scan in a
-- subsequent operation. The results also include the number of items
-- exceeding the limit. A scan can result in no table data meeting the filter
-- criteria. The result set is eventually consistent. By default, Scan
-- operations proceed sequentially; however, for faster performance on large
-- tables, applications can request a parallel Scan by specifying the Segment
-- and TotalSegments parameters. For more information, see Parallel Scan in
-- the Amazon DynamoDB Developer Guide. Returning All Items This example
-- returns all of the items in a table. No scan filter is applied. {
-- "ConsumedCapacity": { "CapacityUnits": 0.5, "TableName": "Reply" },
-- "Count": 2, "Items": [ { "PostedBy": { "S": "joe@example.com" },
-- "ReplyDateTime": { "S": "20130320115336" }, "Id": { "S": "Amazon
-- DynamoDB#How do I update multiple items?" }, "Message": { "S": "Have you
-- looked at the BatchWriteItem API?" } }, { "PostedBy": { "S":
-- "joe@example.com" }, "ReplyDateTime": { "S": "20130320115347" }, "Id": {
-- "S": "Amazon DynamoDB#How do I update multiple items?" }, "Message": { "S":
-- "BatchWriteItem is documented in the Amazon DynamoDB API Reference." } } ],
-- "ScannedCount": 4 }.
module Network.AWS.DynamoDB.Scan
    (
    -- * Request
      Scan
    -- ** Request constructor
    , mkScan
    -- ** Request lenses
    , sTableName
    , sAttributesToGet
    , sLimit
    , sSelect
    , sScanFilter
    , sConditionalOperator
    , sExclusiveStartKey
    , sReturnConsumedCapacity
    , sTotalSegments
    , sSegment

    -- * Response
    , ScanResponse
    -- ** Response constructor
    , mkScanResponse
    -- ** Response lenses
    , srItems
    , srCount
    , srScannedCount
    , srLastEvaluatedKey
    , srConsumedCapacity
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a Scan operation.
data Scan = Scan
    { _sTableName :: !Text
    , _sAttributesToGet :: Maybe (List1 Text)
    , _sLimit :: !(Maybe Integer)
    , _sSelect :: Maybe Select
    , _sScanFilter :: Map Text Condition
    , _sConditionalOperator :: Maybe ConditionalOperator
    , _sExclusiveStartKey :: Map Text AttributeValue
    , _sReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
    , _sTotalSegments :: !(Maybe Integer)
    , _sSegment :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Scan' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Text@
--
-- * @AttributesToGet ::@ @Maybe (List1 Text)@
--
-- * @Limit ::@ @Maybe Integer@
--
-- * @Select ::@ @Maybe Select@
--
-- * @ScanFilter ::@ @Map Text Condition@
--
-- * @ConditionalOperator ::@ @Maybe ConditionalOperator@
--
-- * @ExclusiveStartKey ::@ @Map Text AttributeValue@
--
-- * @ReturnConsumedCapacity ::@ @Maybe ReturnConsumedCapacity@
--
-- * @TotalSegments ::@ @Maybe Integer@
--
-- * @Segment ::@ @Maybe Integer@
--
mkScan :: Text -- ^ 'sTableName'
       -> Scan
mkScan p1 = Scan
    { _sTableName = p1
    , _sAttributesToGet = Nothing
    , _sLimit = Nothing
    , _sSelect = Nothing
    , _sScanFilter = mempty
    , _sConditionalOperator = Nothing
    , _sExclusiveStartKey = mempty
    , _sReturnConsumedCapacity = Nothing
    , _sTotalSegments = Nothing
    , _sSegment = Nothing
    }

-- | The name of the table containing the requested items.
sTableName :: Lens' Scan Text
sTableName = lens _sTableName (\s a -> s { _sTableName = a })

-- | The names of one or more attributes to retrieve. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
sAttributesToGet :: Lens' Scan (Maybe (List1 Text))
sAttributesToGet =
    lens _sAttributesToGet (\s a -> s { _sAttributesToGet = a })

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the limit
-- while processing the results, it stops the operation and returns the
-- matching values up to that point, and a LastEvaluatedKey to apply in a
-- subsequent operation, so that you can pick up where you left off. Also, if
-- the processed data set size exceeds 1 MB before DynamoDB reaches this
-- limit, it stops the operation and returns the matching values up to the
-- limit, and a LastEvaluatedKey to apply in a subsequent operation to
-- continue the operation. For more information see
-- href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html"
-- >Query and Scan in the Amazon DynamoDB Developer Guide.
sLimit :: Lens' Scan (Maybe Integer)
sLimit = lens _sLimit (\s a -> s { _sLimit = a })

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, or the count of matching items.
-- ALL_ATTRIBUTES: Returns all of the item attributes. COUNT: Returns the
-- number of matching items, rather than the matching items themselves.
-- SPECIFIC_ATTRIBUTES : Returns only the attributes listed in
-- AttributesToGet. This is equivalent to specifying AttributesToGet without
-- specifying any value for Select. If neither Select nor AttributesToGet are
-- specified, DynamoDB defaults to ALL_ATTRIBUTES. You cannot use both Select
-- and AttributesToGet together in a single request, unless the value for
-- Select is SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
-- AttributesToGet without any value for Select.).
sSelect :: Lens' Scan (Maybe Select)
sSelect = lens _sSelect (\s a -> s { _sSelect = a })

-- | Evaluates the scan results and returns only the desired values. Multiple
-- conditions are treated as "AND" operations: all conditions must be met to
-- be included in the results. Each ScanConditions element consists of an
-- attribute name to compare, along with the following: AttributeValueList -
-- One or more values to evaluate against the supplied attribute. This list
-- contains exactly one value, except for a BETWEEN or IN comparison, in which
-- case the list contains two values. For type Number, value comparisons are
-- numeric. String value comparisons for greater than, equals, or less than
-- are based on ASCII character code values. For example, a is greater than A,
-- and aa is greater than B. For a list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For Binary,
-- DynamoDB treats each byte of the binary data as unsigned when it compares
-- binary values, for example when evaluating query expressions.
-- ComparisonOperator - A comparator for evaluating attributes. For example,
-- equals, greater than, less than, etc. Valid comparison operators for Scan:
-- EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |
-- BEGINS_WITH | IN | BETWEEN For information on specifying data types in
-- JSON, see JSON Data Format in the Amazon DynamoDB Developer Guide. The
-- following are descriptions of each comparison operator. EQ : Equal.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set). If an item contains an AttributeValue of a
-- different type than the one specified in the request, the value does not
-- match. For example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"}
-- does not equal {"NS":["6", "2", "1"]}. NE : Not equal. AttributeValueList
-- can contain only one AttributeValue of type String, Number, or Binary (not
-- a set). If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not equal
-- {"NS":["6", "2", "1"]}. LE : Less than or equal. AttributeValueList can
-- contain only one AttributeValue of type String, Number, or Binary (not a
-- set). If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not compare to
-- {"NS":["6", "2", "1"]}. LT : Less than. AttributeValueList can contain only
-- one AttributeValue of type String, Number, or Binary (not a set). If an
-- item contains an AttributeValue of a different type than the one specified
-- in the request, the value does not match. For example, {"S":"6"} does not
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
sScanFilter :: Lens' Scan (Map Text Condition)
sScanFilter = lens _sScanFilter (\s a -> s { _sScanFilter = a })

sConditionalOperator :: Lens' Scan (Maybe ConditionalOperator)
sConditionalOperator =
    lens _sConditionalOperator (\s a -> s { _sConditionalOperator = a })

-- | The primary key of the first item that this operation will evalute. Use the
-- value that was returned for LastEvaluatedKey in the previous operation. The
-- data type for ExclusiveStartKey must be String, Number or Binary. No set
-- data types are allowed. In a parallel scan, a Scan request that includes
-- ExclusiveStartKey must specify the same segment whose previous Scan
-- returned the corresponding value of LastEvaluatedKey.
sExclusiveStartKey :: Lens' Scan (Map Text AttributeValue)
sExclusiveStartKey =
    lens _sExclusiveStartKey (\s a -> s { _sExclusiveStartKey = a })

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
sReturnConsumedCapacity :: Lens' Scan (Maybe ReturnConsumedCapacity)
sReturnConsumedCapacity =
    lens _sReturnConsumedCapacity
         (\s a -> s { _sReturnConsumedCapacity = a })

-- | For a parallel Scan request, TotalSegments represents the total number of
-- segments into which the Scan operation will be divided. The value of
-- TotalSegments corresponds to the number of application workers that will
-- perform the parallel scan. For example, if you want to scan a table using
-- four application threads, you would specify a TotalSegments value of 4. The
-- value for TotalSegments must be greater than or equal to 1, and less than
-- or equal to 4096. If you specify a TotalSegments value of 1, the Scan will
-- be sequential rather than parallel. If you specify TotalSegments, you must
-- also specify Segment.
sTotalSegments :: Lens' Scan (Maybe Integer)
sTotalSegments = lens _sTotalSegments (\s a -> s { _sTotalSegments = a })

-- | For a parallel Scan request, Segment identifies an individual segment to be
-- scanned by an application worker. Segment IDs are zero-based, so the first
-- segment is always 0. For example, if you want to scan a table using four
-- application threads, the first thread would specify a Segment value of 0,
-- the second thread would specify 1, and so on. The value of LastEvaluatedKey
-- returned from a parallel Scan request must be used as ExclusiveStartKey
-- with the same Segment ID in a subsequent Scan operation. The value for
-- Segment must be greater than or equal to 0, and less than the value
-- provided for TotalSegments. If you specify Segment, you must also specify
-- TotalSegments.
sSegment :: Lens' Scan (Maybe Integer)
sSegment = lens _sSegment (\s a -> s { _sSegment = a })

instance ToPath Scan

instance ToQuery Scan

instance ToHeaders Scan

instance ToJSON Scan

-- | Represents the output of a Scan operation.
data ScanResponse = ScanResponse
    { _srItems :: [Map Text AttributeValue]
    , _srCount :: !(Maybe Integer)
    , _srScannedCount :: !(Maybe Integer)
    , _srLastEvaluatedKey :: Map Text AttributeValue
    , _srConsumedCapacity :: Maybe ConsumedCapacity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ScanResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Items ::@ @[Map Text AttributeValue]@
--
-- * @Count ::@ @Maybe Integer@
--
-- * @ScannedCount ::@ @Maybe Integer@
--
-- * @LastEvaluatedKey ::@ @Map Text AttributeValue@
--
-- * @ConsumedCapacity ::@ @Maybe ConsumedCapacity@
--
mkScanResponse :: ScanResponse
mkScanResponse = ScanResponse
    { _srItems = mempty
    , _srCount = Nothing
    , _srScannedCount = Nothing
    , _srLastEvaluatedKey = mempty
    , _srConsumedCapacity = Nothing
    }

-- | An array of item attributes that match the scan criteria. Each element in
-- this array consists of an attribute name and the value for that attribute.
srItems :: Lens' ScanResponse [Map Text AttributeValue]
srItems = lens _srItems (\s a -> s { _srItems = a })

-- | The number of items in the response.
srCount :: Lens' ScanResponse (Maybe Integer)
srCount = lens _srCount (\s a -> s { _srCount = a })

-- | The number of items in the complete scan, before any filters are applied. A
-- high ScannedCount value with few, or no, Count results indicates an
-- inefficient Scan operation. For more information, see Count and
-- ScannedCount in the Amazon DynamoDB Developer Guide.
srScannedCount :: Lens' ScanResponse (Maybe Integer)
srScannedCount = lens _srScannedCount (\s a -> s { _srScannedCount = a })

-- | The primary key of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request. If LastEvaluatedKey is null, then the "last
-- page" of results has been processed and there is no more data to be
-- retrieved. If LastEvaluatedKey is anything other than null, this does not
-- necessarily mean that there is more data in the result set. The only way to
-- know when you have reached the end of the result set is when
-- LastEvaluatedKey is null.
srLastEvaluatedKey :: Lens' ScanResponse (Map Text AttributeValue)
srLastEvaluatedKey =
    lens _srLastEvaluatedKey (\s a -> s { _srLastEvaluatedKey = a })

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
srConsumedCapacity :: Lens' ScanResponse (Maybe ConsumedCapacity)
srConsumedCapacity =
    lens _srConsumedCapacity (\s a -> s { _srConsumedCapacity = a })

instance FromJSON ScanResponse

instance AWSRequest Scan where
    type Sv Scan = DynamoDB
    type Rs Scan = ScanResponse

    request = get
    response _ = jsonResponse

instance AWSPager Scan where
    next rq rs
        | nullMap k = Nothing
        | otherwise = Just (rq & sExclusiveStartKey .~ k)
      where
        k = rs ^. srLastEvaluatedKey