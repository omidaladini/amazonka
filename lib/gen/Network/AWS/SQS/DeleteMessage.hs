{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.DeleteMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteMessage action unconditionally removes the specified message from
-- the specified queue. Even if the message is locked by another reader due to
-- the visibility timeout setting, it is still deleted from the queue.
module Network.AWS.SQS.DeleteMessage where

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
deleteMessage :: Text
              -> Text
              -> AWS (Either SQSError DeleteMessageResponse)
deleteMessage p1 p2 = undefined $ DeleteMessage
    { dmrQueueUrl = p1
    , dmrReceiptHandle = p2
    }

data DeleteMessage = DeleteMessage
    { dmrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    , dmrReceiptHandle :: !Text
      -- ^ The receipt handle associated with the message to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteMessage

instance AWSRequest DeleteMessage where
    type Er DeleteMessage = SQSError
    type Rs DeleteMessage = DeleteMessageResponse
    request = getQuery service "DeleteMessage"

data DeleteMessageResponse = DeleteMessageResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteMessageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteMessageResponse"
        :| ["DeleteMessageResult"]
