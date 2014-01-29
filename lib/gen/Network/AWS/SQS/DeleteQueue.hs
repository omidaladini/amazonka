{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.DeleteQueue
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action unconditionally deletes the queue specified by the queue URL.
-- Use this operation WITH CARE! The queue is deleted even if it is NOT empty.
-- Once a queue has been deleted, the queue name is unavailable for use with
-- new queues for 60 seconds.
module Network.AWS.SQS.DeleteQueue where

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
deleteQueue :: Text
            -> AWS (Either SQSError DeleteQueueResponse)
deleteQueue p1 = undefined $ DeleteQueue
    { dqrQueueUrl = p1
    }

data DeleteQueue = DeleteQueue
    { dqrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteQueue

instance AWSRequest DeleteQueue where
    type Er DeleteQueue = SQSError
    type Rs DeleteQueue = DeleteQueueResponse
    request = getQuery service "DeleteQueue"

data DeleteQueueResponse = DeleteQueueResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteQueueResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteQueueResponse"
        :| ["DeleteQueueResult"]
