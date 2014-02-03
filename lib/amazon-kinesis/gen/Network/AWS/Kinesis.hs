-- Module      : Network.AWS.Kinesis
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Kinesis
    (
    -- * Operations
    -- ** PutRecord
      module Network.AWS.Kinesis.PutRecord
    -- ** MergeShards
    , module Network.AWS.Kinesis.MergeShards
    -- ** GetShardIterator
    , module Network.AWS.Kinesis.GetShardIterator
    -- ** GetRecords
    , module Network.AWS.Kinesis.GetRecords
    -- ** DeleteStream
    , module Network.AWS.Kinesis.DeleteStream
    -- ** ListStreams
    , module Network.AWS.Kinesis.ListStreams
    -- ** CreateStream
    , module Network.AWS.Kinesis.CreateStream
    -- ** SplitShard
    , module Network.AWS.Kinesis.SplitShard
    -- ** DescribeStream
    , module Network.AWS.Kinesis.DescribeStream

    -- * Types
    -- ** StreamDescription
    , StreamDescription (..)
    -- ** Shard
    , Shard (..)
    -- ** SequenceNumberRange
    , SequenceNumberRange (..)
    -- ** Record
    , Record (..)
    -- ** HashKeyRange
    , HashKeyRange (..)
    -- ** StreamStatus
    , StreamStatus (..)
    -- ** ShardIteratorType
    , ShardIteratorType (..)

    -- * Errors
    , KinesisError (..)
    ) where

import Network.AWS.Kinesis.Service
import Network.AWS.Kinesis.Types

import Network.AWS.Kinesis.PutRecord
import Network.AWS.Kinesis.MergeShards
import Network.AWS.Kinesis.GetShardIterator
import Network.AWS.Kinesis.GetRecords
import Network.AWS.Kinesis.DeleteStream
import Network.AWS.Kinesis.ListStreams
import Network.AWS.Kinesis.CreateStream
import Network.AWS.Kinesis.SplitShard
import Network.AWS.Kinesis.DescribeStream
