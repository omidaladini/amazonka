{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SQS.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.SQS.Service

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry
    { smbrfId :: !Text
      -- ^ An identifier for the message in this batch.
    , smbrfMD5OfMessageBody :: !Text
      -- ^ An MD5 digest of the non-URL-encoded message body string. This can be used
      -- to verify that SQS received the message correctly. SQS first URL decodes
      -- the message before creating the MD5 digest. For information about MD5, go
      -- to http://faqs.org/rfcs/rfc1321.html.
    , smbrfMessageId :: !Text
      -- ^ An identifier for the message.
    } deriving (Eq, Show, Generic)

instance ToQuery SendMessageBatchResultEntry

instance FromXML SendMessageBatchResultEntry where
    fromXMLOptions = xmlOptions

instance ToXML SendMessageBatchResultEntry where
    toXMLOptions = xmlOptions

-- | Contains the details of a single SQS message along with a Id.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry
    { smbreDelaySeconds :: Maybe Int
      -- ^ The number of seconds for which the message has to be delayed.
    , smbreId :: !Text
      -- ^ An identifier for the message in this batch. This is used to communicate
      -- the result. Note that the the Ids of a batch request need to be unique
      -- within the request.
    , smbreMessageBody :: !Text
      -- ^ Body of the message.
    } deriving (Eq, Show, Generic)

instance ToQuery SendMessageBatchRequestEntry

instance FromXML SendMessageBatchRequestEntry where
    fromXMLOptions = xmlOptions

instance ToXML SendMessageBatchRequestEntry where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Message
data Message = Message
    { mAttributes :: HashMap QueueAttributeName Text
    , mBody :: Maybe Text
    , mMD5OfBody :: Maybe Text
    , mMessageId :: Maybe Text
    , mReceiptHandle :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Message

instance FromXML Message where
    fromXMLOptions = xmlOptions

instance ToXML Message where
    toXMLOptions = xmlOptions

-- | Encloses the id an entry in DeleteMessageBatch.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry
    { dmbrfId :: Text
      -- ^ Represents a successfully deleted message.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteMessageBatchResultEntry

instance FromXML DeleteMessageBatchResultEntry where
    fromXMLOptions = xmlOptions

instance ToXML DeleteMessageBatchResultEntry where
    toXMLOptions = xmlOptions

-- | Encloses a receipt handle and an identifier for it.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry
    { dmbreId :: !Text
      -- ^ An identifier for this particular receipt handle. This is used to
      -- communicate the result. Note that the Ids of a batch request need to be
      -- unique within the request.
    , dmbreReceiptHandle :: !Text
      -- ^ A receipt handle.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteMessageBatchRequestEntry

instance FromXML DeleteMessageBatchRequestEntry where
    fromXMLOptions = xmlOptions

instance ToXML DeleteMessageBatchRequestEntry where
    toXMLOptions = xmlOptions

-- | Encloses the id of an entry in ChangeMessageVisibilityBatch.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry
    { cmvbrfId :: Text
      -- ^ Represents a message whose visibility timeout has been changed
      -- successfully.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeMessageVisibilityBatchResultEntry

instance FromXML ChangeMessageVisibilityBatchResultEntry where
    fromXMLOptions = xmlOptions

instance ToXML ChangeMessageVisibilityBatchResultEntry where
    toXMLOptions = xmlOptions

-- | Encloses a receipt handle and an entry id for each message in
-- ChangeMessageVisibilityBatch.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry
    { cmvbreId :: !Text
      -- ^ An identifier for this particular receipt handle. This is used to
      -- communicate the result. Note that the Ids of a batch request need to be
      -- unique within the request.
    , cmvbreReceiptHandle :: !Text
      -- ^ A receipt handle.
    , cmvbreVisibilityTimeout :: Maybe Int
      -- ^ The new value (in seconds) for the message's visibility timeout.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeMessageVisibilityBatchRequestEntry

instance FromXML ChangeMessageVisibilityBatchRequestEntry where
    fromXMLOptions = xmlOptions

instance ToXML ChangeMessageVisibilityBatchRequestEntry where
    toXMLOptions = xmlOptions

-- | This is used in the responses of batch API to give a detailed description
-- of the result of an operation on each entry in the request.
data BatchResultErrorEntry = BatchResultErrorEntry
    { breeCode :: !Text
      -- ^ An error code representing why the operation failed on this entry.
    , breeId :: !Text
      -- ^ The id of an entry in a batch request.
    , breeMessage :: Maybe Text
      -- ^ A message explaining why the operation failed on this entry.
    , breeSenderFault :: !Bool
      -- ^ Whether the error happened due to the sender's fault.
    } deriving (Eq, Show, Generic)

instance ToQuery BatchResultErrorEntry

instance FromXML BatchResultErrorEntry where
    fromXMLOptions = xmlOptions

instance ToXML BatchResultErrorEntry where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for QueueAttributeName
data QueueAttributeName
    = QueueAttributeNameApproximateNumberOfMessages
    | QueueAttributeNameApproximateNumberOfMessagesDelayed
    | QueueAttributeNameApproximateNumberOfMessagesNotVisible
    | QueueAttributeNameCreatedTimestamp
    | QueueAttributeNameDelaySeconds
    | QueueAttributeNameLastModifiedTimestamp
    | QueueAttributeNameMaximumMessageSize
    | QueueAttributeNameMessageRetentionPeriod
    | QueueAttributeNamePolicy
    | QueueAttributeNameQueueArn
    | QueueAttributeNameReceiveMessageWaitTimeSeconds
    | QueueAttributeNameVisibilityTimeout
      deriving (Eq, Ord, Generic)

instance Hashable QueueAttributeName

instance FromText QueueAttributeName where
    fromText "ApproximateNumberOfMessages" = Right QueueAttributeNameApproximateNumberOfMessages
    fromText "ApproximateNumberOfMessagesDelayed" = Right QueueAttributeNameApproximateNumberOfMessagesDelayed
    fromText "ApproximateNumberOfMessagesNotVisible" = Right QueueAttributeNameApproximateNumberOfMessagesNotVisible
    fromText "CreatedTimestamp" = Right QueueAttributeNameCreatedTimestamp
    fromText "DelaySeconds" = Right QueueAttributeNameDelaySeconds
    fromText "LastModifiedTimestamp" = Right QueueAttributeNameLastModifiedTimestamp
    fromText "MaximumMessageSize" = Right QueueAttributeNameMaximumMessageSize
    fromText "MessageRetentionPeriod" = Right QueueAttributeNameMessageRetentionPeriod
    fromText "Policy" = Right QueueAttributeNamePolicy
    fromText "QueueArn" = Right QueueAttributeNameQueueArn
    fromText "ReceiveMessageWaitTimeSeconds" = Right QueueAttributeNameReceiveMessageWaitTimeSeconds
    fromText "VisibilityTimeout" = Right QueueAttributeNameVisibilityTimeout
    fromText e = fromTextFail $ "Unrecognised QueueAttributeName: " <> e

instance Read QueueAttributeName where
    readsPrec _ = fromTextRead

instance ToText QueueAttributeName where
    toText QueueAttributeNameApproximateNumberOfMessages = "ApproximateNumberOfMessages"
    toText QueueAttributeNameApproximateNumberOfMessagesDelayed = "ApproximateNumberOfMessagesDelayed"
    toText QueueAttributeNameApproximateNumberOfMessagesNotVisible = "ApproximateNumberOfMessagesNotVisible"
    toText QueueAttributeNameCreatedTimestamp = "CreatedTimestamp"
    toText QueueAttributeNameDelaySeconds = "DelaySeconds"
    toText QueueAttributeNameLastModifiedTimestamp = "LastModifiedTimestamp"
    toText QueueAttributeNameMaximumMessageSize = "MaximumMessageSize"
    toText QueueAttributeNameMessageRetentionPeriod = "MessageRetentionPeriod"
    toText QueueAttributeNamePolicy = "Policy"
    toText QueueAttributeNameQueueArn = "QueueArn"
    toText QueueAttributeNameReceiveMessageWaitTimeSeconds = "ReceiveMessageWaitTimeSeconds"
    toText QueueAttributeNameVisibilityTimeout = "VisibilityTimeout"

instance Show QueueAttributeName where
    show = toTextShow

instance ToQuery QueueAttributeName where
    toQuery = toTextQuery

instance FromXML QueueAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML QueueAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
