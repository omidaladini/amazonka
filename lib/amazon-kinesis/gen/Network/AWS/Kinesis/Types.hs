{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Kinesis.Types where

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

import Network.AWS.Kinesis.Service

-- | Contains the current status of the stream, the stream ARN, an array of
-- shard objects that comprise the stream, and states whether there are more
-- shards available.
data StreamDescription = StreamDescription
    { sdHasMoreShards :: !Bool
      -- ^ If set to true there are more shards in the stream available to describe.
    , sdShards :: [Shard]
      -- ^ The shards that comprise the stream.
    , sdStreamARN :: !Text
      -- ^ The Amazon Resource Name (ARN) for the stream being described.
    , sdStreamName :: !Text
      -- ^ The name of the stream being described.
    , sdStreamStatus :: !StreamStatus
      -- ^ The current status of the stream being described. The stream status is one
      -- of the following states: CREATING - The stream is being created. Upon
      -- receiving a CreateStream request, Amazon Kinesis immediately returns and
      -- sets StreamStatus to CREATING. DELETING - The stream is being deleted.
      -- After a DeleteStream request, the specified stream is in the DELETING state
      -- until Amazon Kinesis completes the deletion. ACTIVE - The stream exists and
      -- is ready for read and write operations or deletion. You should perform read
      -- and write operations only on an ACTIVE stream. UPDATING - Shards in the
      -- stream are being merged or split. Read and write operations continue to
      -- work while the stream is in the UPDATING state.
    } deriving (Eq, Show, Generic)

instance FromJSON StreamDescription
instance ToJSON StreamDescription

-- | A uniquely identified group of data records in an Amazon Kinesis stream.
data Shard = Shard
    { sAdjacentParentShardId :: Maybe Text
      -- ^ The shard Id of the shard adjacent to the shard's parent.
    , sHashKeyRange :: HashKeyRange
      -- ^ The range of possible hash key values for the shard, which is a set of
      -- ordered contiguous positive integers.
    , sParentShardId :: Maybe Text
      -- ^ The shard Id of the shard's parent.
    , sSequenceNumberRange :: SequenceNumberRange
      -- ^ The range of possible sequence numbers for the shard.
    , sShardId :: !Text
      -- ^ The unique identifier of the shard within the Amazon Kinesis stream.
    } deriving (Eq, Show, Generic)

instance FromJSON Shard
instance ToJSON Shard

-- | The range of possible sequence numbers for the shard.
data SequenceNumberRange = SequenceNumberRange
    { snrEndingSequenceNumber :: Maybe Text
      -- ^ The ending sequence number for the range. Shards that are in the OPEN state
      -- have an ending sequence number of null.
    , snrStartingSequenceNumber :: !Text
      -- ^ The starting sequence number for the range.
    } deriving (Eq, Show, Generic)

instance FromJSON SequenceNumberRange
instance ToJSON SequenceNumberRange

-- | The unit of data of the Amazon Kinesis stream, which is composed of a
-- sequence number, a partition key, and a data blob.
data Record = Record
    { rData :: !Blob
      -- ^ The data blob. The data in the blob is both opaque and immutable to the
      -- Amazon Kinesis service, which does not inspect, interpret, or change the
      -- data in the blob in any way. The maximum size of the data blob is 50
      -- kilobytes (KB).
    , rPartitionKey :: !Text
      -- ^ Identifies which shard in the stream the data record is assigned to.
    , rSequenceNumber :: !Text
      -- ^ The unique identifier for the record in the Amazon Kinesis stream.
    } deriving (Eq, Show, Generic)

instance FromJSON Record
instance ToJSON Record

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
data HashKeyRange = HashKeyRange
    { hkrEndingHashKey :: !Text
      -- ^ The ending hash key of the hash key range.
    , hkrStartingHashKey :: !Text
      -- ^ The starting hash key of the hash key range.
    } deriving (Eq, Show, Generic)

instance FromJSON HashKeyRange
instance ToJSON HashKeyRange

-- | The current status of the stream being described. The stream status is one
-- of the following states: CREATING - The stream is being created. Upon
-- receiving a CreateStream request, Amazon Kinesis immediately returns and
-- sets StreamStatus to CREATING. DELETING - The stream is being deleted.
-- After a DeleteStream request, the specified stream is in the DELETING state
-- until Amazon Kinesis completes the deletion. ACTIVE - The stream exists and
-- is ready for read and write operations or deletion. You should perform read
-- and write operations only on an ACTIVE stream. UPDATING - Shards in the
-- stream are being merged or split. Read and write operations continue to
-- work while the stream is in the UPDATING state.

data StreamStatus
    = ACTIVE
    | CREATING
    | DELETING
    | UPDATING
      deriving (Eq, Ord, Generic)

instance Hashable StreamStatus

instance FromText StreamStatus where
    fromText "ACTIVE" = Right ACTIVE
    fromText "CREATING" = Right CREATING
    fromText "DELETING" = Right DELETING
    fromText "UPDATING" = Right UPDATING
    fromText e = fromTextFail $ "Unrecognised StreamStatus: " <> e

instance Read StreamStatus where
    readsPrec _ = fromTextRead

instance ToText StreamStatus where
    toText ACTIVE = "ACTIVE"
    toText CREATING = "CREATING"
    toText DELETING = "DELETING"
    toText UPDATING = "UPDATING"

instance Show StreamStatus where
    show = toTextShow

instance FromJSON StreamStatus where
    parseJSON = fromTextJSON "StreamStatus"

instance FromJSON v => FromJSON (HashMap StreamStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON StreamStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StreamStatus v) where
    toJSON = toTextHashJSON

-- | Determines how the shard iterator is used to start reading data records
-- from the shard. The following are the valid shard iterator types:
-- AT_SEQUENCE_NUMBER - Start reading exactly from the position denoted by a
-- specific sequence number. AFTER_SEQUENCE_NUMBER - Start reading right after
-- the position denoted by a specific sequence number. TRIM_HORIZON - Start
-- reading at the last untrimmed record in the shard in the system, which is
-- the oldest data record in the shard. LATEST - Start reading just after the
-- most recent record in the shard, so that you always read the most recent
-- data in the shard.

data ShardIteratorType
    = AFTER_SEQUENCE_NUMBER
    | AT_SEQUENCE_NUMBER
    | LATEST
    | TRIM_HORIZON
      deriving (Eq, Ord, Generic)

instance Hashable ShardIteratorType

instance FromText ShardIteratorType where
    fromText "AFTER_SEQUENCE_NUMBER" = Right AFTER_SEQUENCE_NUMBER
    fromText "AT_SEQUENCE_NUMBER" = Right AT_SEQUENCE_NUMBER
    fromText "LATEST" = Right LATEST
    fromText "TRIM_HORIZON" = Right TRIM_HORIZON
    fromText e = fromTextFail $ "Unrecognised ShardIteratorType: " <> e

instance Read ShardIteratorType where
    readsPrec _ = fromTextRead

instance ToText ShardIteratorType where
    toText AFTER_SEQUENCE_NUMBER = "AFTER_SEQUENCE_NUMBER"
    toText AT_SEQUENCE_NUMBER = "AT_SEQUENCE_NUMBER"
    toText LATEST = "LATEST"
    toText TRIM_HORIZON = "TRIM_HORIZON"

instance Show ShardIteratorType where
    show = toTextShow

instance FromJSON ShardIteratorType where
    parseJSON = fromTextJSON "ShardIteratorType"

instance FromJSON v => FromJSON (HashMap ShardIteratorType v) where
    parseJSON = fromTextHashJSON

instance ToJSON ShardIteratorType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ShardIteratorType v) where
    toJSON = toTextHashJSON
