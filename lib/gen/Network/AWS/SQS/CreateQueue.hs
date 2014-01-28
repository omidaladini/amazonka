{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.CreateQueue
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateQueue action creates a new queue, or returns the URL of an
-- existing one. When you request CreateQueue, you provide a name for the
-- queue. To successfully create a new queue, you must provide a name that is
-- unique within the scope of your own queues. You may pass one or more
-- attributes in the request. If you do not provide a value for any attribute,
-- the queue will have the default value for that attribute. Permitted
-- attributes are the same that can be set using SetQueueAttributes. If you
-- provide the name of an existing queue, a new queue isn't created. If the
-- values of attributes provided with the request match up with those on the
-- existing queue, the queue URL is returned. Otherwise, a QueueNameExists
-- error is returned.
module Network.AWS.SQS.CreateQueue where

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

data CreateQueue = CreateQueue
    { cqrAttributes :: HashMap QueueAttributeName Text
      -- ^ A map of attributes with their corresponding values.
    , cqrQueueName :: !Text
      -- ^ The name for the queue to be created.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateQueue

instance AWSRequest CreateQueue where
    type Er CreateQueue = SQSError
    type Rs CreateQueue = CreateQueueResponse
    request = getQuery service "CreateQueue"

data CreateQueueResponse = CreateQueueResponse
    { cqrrsQueueUrl :: Maybe Text
      -- ^ The URL for the created SQS queue.
    } deriving (Eq, Show, Generic)

instance FromXML CreateQueueResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateQueueResponse"
        :| ["CreateQueueResult"]
