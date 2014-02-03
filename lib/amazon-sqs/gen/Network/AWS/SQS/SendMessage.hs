{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The SendMessage action delivers a message to the specified queue.
module Network.AWS.SQS.SendMessage where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
sendMessage :: Text
            -> Text
            -> SendMessage
sendMessage p1 p2 = SendMessage
    { smrMessageBody = p1
    , smrQueueUrl = p2
    , smrDelaySeconds = Nothing
    }

data SendMessage = SendMessage
    { smrDelaySeconds :: Maybe Int
      -- ^ The number of seconds the message has to be delayed.
    , smrMessageBody :: !Text
      -- ^ The message to send.
    , smrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery SendMessage

instance AWSRequest SendMessage where
    type Er SendMessage = SQSError
    type Rs SendMessage = SendMessageResponse
    request = getQuery service "SendMessage"

data SendMessageResponse = SendMessageResponse
    { smrrsMD5OfMessageBody :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message body string. This can be used
      -- to verify that SQS received the message correctly. SQS first URL decodes
      -- the message before creating the MD5 digest. For information about MD5, go
      -- to http://faqs.org/rfcs/rfc1321.html.
    , smrrsMessageId :: Maybe Text
      -- ^ The message ID of the message added to the queue.
    } deriving (Eq, Show, Generic)

instance FromXML SendMessageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SendMessageResponse"
        :| ["SendMessageResult"]
