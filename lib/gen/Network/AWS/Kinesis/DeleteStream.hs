{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes a stream and all of its shards and data. You must
-- shut down any applications that are operating on the stream before you
-- delete the stream. If an application attempts to operate on a deleted
-- stream, it will receive the exception ResourceNotFoundException. If the
-- stream is in the ACTIVE state, you can delete it. After a DeleteStream
-- request, the specified stream is in the DELETING state until Amazon Kinesis
-- completes the deletion. Note: Amazon Kinesis might continue to accept data
-- read and write operations, such as PutRecord and GetRecords, on a stream in
-- the DELETING state until the stream deletion is complete. When you delete a
-- stream, any shards in that stream are also deleted. You can use the
-- DescribeStream operation to check the state of the stream, which is
-- returned in StreamStatus. DeleteStream has a limit of 5 transactions per
-- second per account. Delete a Stream The following is an example of an
-- Amazon Kinesis DeleteStream request and response. POST / HTTP/1.1 Host:
-- kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.DeleteStream { "StreamName":"exampleStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.Kinesis.DeleteStream where

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

data DeleteStream = DeleteStream
    { dsiStreamName :: !Text
      -- ^ The name of the stream to delete.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteStream

instance AWSRequest DeleteStream where
    type Er DeleteStream = KinesisError
    type Rs DeleteStream = DeleteStreamResponse
    request  = getJSON service
    response = responseJSON

data DeleteStreamResponse = DeleteStreamResponse
    deriving (Eq, Show, Generic)

instance FromJSON DeleteStreamResponse
