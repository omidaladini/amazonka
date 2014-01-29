{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.PutRecord
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation puts a data record into an Amazon Kinesis stream from a
-- producer. This operation must be called to send data from the producer into
-- the Amazon Kinesis stream for real-time ingestion and subsequent
-- processing. The PutRecord operation requires the name of the stream that
-- captures, stores, and transports the data; a partition key; and the data
-- blob itself. The data blob could be a segment from a log file,
-- geographic/location data, website clickstream data, or any other data type.
-- The partition key is used to distribute data across shards. Amazon Kinesis
-- segregates the data records that belong to a data stream into multiple
-- shards, using the partition key associated with each data record to
-- determine which shard a given data record belongs to. Partition keys are
-- Unicode strings, with a maximum length limit of 256 bytes. An MD5 hash
-- function is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards using the hash key ranges of the shards.
-- You can override hashing the partition key to determine the shard by
-- explicitly specifying a hash value using the ExplicitHashKey parameter. For
-- more information, see the Amazon Kinesis Developer Guide. PutRecord returns
-- the shard ID of where the data record was placed and the sequence number
-- that was assigned to the data record. The SequenceNumberForOrdering sets
-- the initial sequence number for the partition key. Later PutRecord requests
-- to the same partition key (from the same client) will automatically
-- increase from SequenceNumberForOrdering, ensuring strict sequential
-- ordering. If a PutRecord request cannot be processed because of
-- insufficient provisioned throughput on the shard involved in the request,
-- PutRecord throws ProvisionedThroughputExceededException. Data records are
-- accessible for only 24 hours from the time that they are added to an Amazon
-- Kinesis stream. Add Data to a Stream The following is an example of an
-- Amazon Kinesis PutRecord request and response. POST / HTTP/1.1 Host:
-- kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.PutRecord { "StreamName": "exampleStreamName", "Data":
-- "XzxkYXRhPl8x", "PartitionKey": "partitionKey" } HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]> { "SequenceNumber": "21269319989653637946712965403778482177",
-- "ShardId": "shardId-000000000001" }.
module Network.AWS.Kinesis.PutRecord where

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

import Network.AWS.Kinesis.Service
import Network.AWS.Kinesis.Types

-- | Convenience method utilising default fields where applicable.
putRecord :: Blob
          -> Text
          -> Text
          -> AWS (Either KinesisError PutRecordResponse)
putRecord p1 p2 p3 = undefined $ PutRecord
    { priData = p1
    , priPartitionKey = p2
    , priStreamName = p3
    , priExplicitHashKey = Nothing
    , priSequenceNumberForOrdering = Nothing
    }

data PutRecord = PutRecord
    { priData :: !Blob
      -- ^ The data blob to put into the record, which must be Base64 encoded. The
      -- maximum size of the data blob is 50 kilobytes (KB).
    , priExplicitHashKey :: Maybe Text
      -- ^ The hash value used to explicitly determine the shard the data record is
      -- assigned to by overriding the partition key hash.
    , priPartitionKey :: !Text
      -- ^ Determines which shard in the stream the data record is assigned to.
      -- Partition keys are Unicode strings with a maximum length limit of 256
      -- bytes. Amazon Kinesis uses the partition key as input to a hash function
      -- that maps the partition key and associated data to a specific shard.
      -- Specifically, an MD5 hash function is used to map partition keys to 128-bit
      -- integer values and to map associated data records to shards. As a result of
      -- this hashing mechanism, all data records with the same partition key will
      -- map to the same shard within the stream.
    , priSequenceNumberForOrdering :: Maybe Text
      -- ^ The sequence number to use as the initial number for the partition key.
      -- Subsequent calls to PutRecord from the same client and for the same
      -- partition key will increase from the SequenceNumberForOrdering value.
    , priStreamName :: !Text
      -- ^ The name of the stream to put the data record into.
    } deriving (Eq, Show, Generic)

instance ToJSON PutRecord

instance AWSRequest PutRecord where
    type Er PutRecord = KinesisError
    type Rs PutRecord = PutRecordResponse
    request  = getJSON service
    response = responseJSON

data PutRecordResponse = PutRecordResponse
    { prirsSequenceNumber :: !Text
      -- ^ The sequence number identifier that was assigned to the put data record.
      -- The sequence number for the record is unique across all records in the
      -- stream. A sequence number is the identifier associated with every record
      -- put into the stream.
    , prirsShardId :: !Text
      -- ^ The shard ID of the shard where the data record was placed.
    } deriving (Eq, Show, Generic)

instance FromJSON PutRecordResponse
