{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This is a batch version of SendMessage. It takes multiple messages and adds
-- each of them to the queue. The result of each add operation is reported
-- individually in the response.
module Network.AWS.SQS.SendMessageBatch where

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
sendMessageBatch :: [SendMessageBatchRequestEntry]
                 -> Text
                 -> AWS (Either SQSError SendMessageBatchResponse)
sendMessageBatch p1 p2 = undefined $ SendMessageBatch
    { smbrEntries = p1
    , smbrQueueUrl = p2
    }

data SendMessageBatch = SendMessageBatch
    { smbrEntries :: [SendMessageBatchRequestEntry]
      -- ^ A list of SendMessageBatchRequestEntrys.
    , smbrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery SendMessageBatch

instance AWSRequest SendMessageBatch where
    type Er SendMessageBatch = SQSError
    type Rs SendMessageBatch = SendMessageBatchResponse
    request = getQuery service "SendMessageBatch"

data SendMessageBatchResponse = SendMessageBatchResponse
    { smbrrsFailed :: [BatchResultErrorEntry]
      -- ^ A list of BatchResultErrorEntrys with the error detail about each message
      -- that could not be enqueued.
    , smbrrsSuccessful :: [SendMessageBatchResultEntry]
      -- ^ A list of SendMessageBatchResultEntrys.
    } deriving (Eq, Show, Generic)

instance FromXML SendMessageBatchResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SendMessageBatchResponse"
        :| ["SendMessageBatchResult"]
