{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets attributes for the specified queue. The following attributes are
-- supported: All - returns all values. ApproximateNumberOfMessages - returns
-- the approximate number of visible messages in a queue. For more
-- information, see Resources Required to Process Messages in the Amazon SQS
-- Developer Guide. ApproximateNumberOfMessagesNotVisible - returns the
-- approximate number of messages that are not timed-out and not deleted. For
-- more information, see Resources Required to Process Messages in the Amazon
-- SQS Developer Guide. VisibilityTimeout - returns the visibility timeout for
-- the queue. For more information about visibility timeout, see Visibility
-- Timeout in the Amazon SQS Developer Guide. CreatedTimestamp - returns the
-- time when the queue was created (epoch time in seconds).
-- LastModifiedTimestamp - returns the time when the queue was last changed
-- (epoch time in seconds). Policy - returns the queue's policy.
-- MaximumMessageSize - returns the limit of how many bytes a message can
-- contain before Amazon SQS rejects it. MessageRetentionPeriod - returns the
-- number of seconds Amazon SQS retains a message. QueueArn - returns the
-- queue's Amazon resource name (ARN). ApproximateNumberOfMessagesDelayed -
-- returns the approximate number of messages that are pending to be added to
-- the queue. DelaySeconds - returns the default delay on the queue in
-- seconds. ReceiveMessageWaitTimeSeconds - returns the time for which a
-- ReceiveMessage call will wait for a message to arrive.
module Network.AWS.SQS.GetQueueAttributes where

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
getQueueAttributes :: Text
                   -> AWS (Either SQSError GetQueueAttributesResponse)
getQueueAttributes p1 = undefined $ GetQueueAttributes
    { gqarQueueUrl = p1
    , gqarAttributeNames = []
    }

data GetQueueAttributes = GetQueueAttributes
    { gqarAttributeNames :: [QueueAttributeName]
      -- ^ A list of attributes to retrieve information for.
    , gqarQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery GetQueueAttributes

instance AWSRequest GetQueueAttributes where
    type Er GetQueueAttributes = SQSError
    type Rs GetQueueAttributes = GetQueueAttributesResponse
    request = getQuery service "GetQueueAttributes"

data GetQueueAttributesResponse = GetQueueAttributesResponse
    { gqarrsAttributes :: HashMap QueueAttributeName Text
      -- ^ A map of attributes to the respective values.
    } deriving (Eq, Show, Generic)

instance FromXML GetQueueAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetQueueAttributesResponse"
        :| ["GetQueueAttributesResult"]
