{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves one or more messages from the specified queue, including the
-- message body and message ID of each message. Messages returned by this
-- action stay in the queue until you delete them. However, once a message is
-- returned to a ReceiveMessage request, it is not returned on subsequent
-- ReceiveMessage requests for the duration of the VisibilityTimeout. If you
-- do not specify a VisibilityTimeout in the request, the overall visibility
-- timeout for the queue is used for the returned messages. If a message is
-- available in the queue, the call will return immediately. Otherwise, it
-- will wait up to WaitTimeSeconds for a message to arrive. If you do not
-- specify WaitTimeSeconds in the request, the queue attribute
-- ReceiveMessageWaitTimeSeconds is used to determine how long to wait. You
-- could ask for additional information about each message through the
-- attributes. Attributes that can be requested are [SenderId,
-- ApproximateFirstReceiveTimestamp, ApproximateReceiveCount, SentTimestamp].
module Network.AWS.SQS.ReceiveMessage where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.SQS.Service
import Network.AWS.SQS.Types

-- | Convenience method utilising default fields where applicable.
receiveMessage :: Text
               -> AWS (Either SQSError ReceiveMessageResponse)
receiveMessage p1 = undefined $ ReceiveMessage
    { rmrQueueUrl = p1
    , rmrAttributeNames = []
    , rmrMaxNumberOfMessages = Nothing
    , rmrVisibilityTimeout = Nothing
    , rmrWaitTimeSeconds = Nothing
    }

data ReceiveMessage = ReceiveMessage
    { rmrAttributeNames :: [QueueAttributeName]
      -- ^ A list of attributes that need to be returned along with each message. The
      -- set of valid attributes are [SenderId, ApproximateFirstReceiveTimestamp,
      -- ApproximateReceiveCount, SentTimestamp].
    , rmrMaxNumberOfMessages :: Maybe Int
      -- ^ The maximum number of messages to return. Amazon SQS never returns more
      -- messages than this value but may return fewer. All of the messages are not
      -- necessarily returned.
    , rmrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    , rmrVisibilityTimeout :: Maybe Int
      -- ^ The duration (in seconds) that the received messages are hidden from
      -- subsequent retrieve requests after being retrieved by a ReceiveMessage
      -- request.
    , rmrWaitTimeSeconds :: Maybe Int
      -- ^ The duration (in seconds) for which the call will wait for a message to
      -- arrive in the queue before returning. If a message is available, the call
      -- will return sooner than WaitTimeSeconds.
    } deriving (Eq, Show, Generic)

instance ToQuery ReceiveMessage

instance AWSRequest ReceiveMessage where
    type Er ReceiveMessage = SQSError
    type Rs ReceiveMessage = ReceiveMessageResponse
    request = getQuery service "ReceiveMessage"

data ReceiveMessageResponse = ReceiveMessageResponse
    { rmrrsMessages :: [Message]
      -- ^ A list of messages.
    } deriving (Eq, Show, Generic)

instance FromXML ReceiveMessageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ReceiveMessageResponse"
        :| ["ReceiveMessageResult"]
