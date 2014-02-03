{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This is a batch version of DeleteMessage. It takes multiple receipt handles
-- and deletes each one of the messages. The result of the delete operation on
-- each message is reported individually in the response.
module Network.AWS.SQS.DeleteMessageBatch where

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
deleteMessageBatch :: [DeleteMessageBatchRequestEntry]
                   -> Text
                   -> DeleteMessageBatch
deleteMessageBatch p1 p2 = DeleteMessageBatch
    { dmbrEntries = p1
    , dmbrQueueUrl = p2
    }

data DeleteMessageBatch = DeleteMessageBatch
    { dmbrEntries :: [DeleteMessageBatchRequestEntry]
      -- ^ A list of receipt handles for the messages to be deleted.
    , dmbrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteMessageBatch

instance AWSRequest DeleteMessageBatch where
    type Er DeleteMessageBatch = SQSError
    type Rs DeleteMessageBatch = DeleteMessageBatchResponse
    request = getQuery service "DeleteMessageBatch"

data DeleteMessageBatchResponse = DeleteMessageBatchResponse
    { dmbrrsFailed :: [BatchResultErrorEntry]
      -- ^ A list of BatchResultErrorEntrys.
    , dmbrrsSuccessful :: [DeleteMessageBatchResultEntry]
      -- ^ A list of DeleteMessageBatchResultEntrys.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteMessageBatchResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteMessageBatchResponse"
        :| ["DeleteMessageBatchResult"]
