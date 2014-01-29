{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns an array of the names of all the streams that are
-- associated with the AWS account making the ListStreams request. A given AWS
-- account can have many streams active at one time. The number of streams may
-- be too large to return from a single call to ListStreams. You can limit the
-- number of returned streams using the Limit parameter. If you do not specify
-- a value for the Limit parameter, Amazon Kinesis uses the default limit,
-- which is currently 10. You can detect if there are more streams available
-- to list by using the HasMoreStreams flag from the returned output. If there
-- are more streams available, you can request more streams by using the name
-- of the last stream returned by the ListStreams request in the
-- ExclusiveStartStreamName parameter in a subsequent request to ListStreams.
-- The group of stream names returned by the subsequent request is then added
-- to the list. You can continue this process until all the stream names have
-- been collected in the list. ListStreams has a limit of 5 transactions per
-- second per account. List the Streams for an AWS Account The following is an
-- example of an Amazon Kinesis ListStreams request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.ListStreams HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]> {
-- "HasMoreStreams": false, "StreamNames": [ "exampleStreamName" ] }.
module Network.AWS.Kinesis.ListStreams where

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
listStreams :: AWS (Either KinesisError ListStreamsResponse)
listStreams = undefined $ ListStreams
    { lsiExclusiveStartStreamName = Nothing
    , lsiLimit = Nothing
    }

data ListStreams = ListStreams
    { lsiExclusiveStartStreamName :: Maybe Text
      -- ^ The name of the stream to start the list with.
    , lsiLimit :: Maybe Int
      -- ^ The maximum number of streams to list.
    } deriving (Eq, Show, Generic)

instance ToJSON ListStreams

instance AWSRequest ListStreams where
    type Er ListStreams = KinesisError
    type Rs ListStreams = ListStreamsResponse
    request  = getJSON service
    response = responseJSON

data ListStreamsResponse = ListStreamsResponse
    { lsirsHasMoreStreams :: !Bool
      -- ^ If set to true, there are more streams available to list.
    , lsirsStreamNames :: [Text]
      -- ^ The names of the streams that are associated with the AWS account making
      -- the ListStreams request.
    } deriving (Eq, Show, Generic)

instance FromJSON ListStreamsResponse
