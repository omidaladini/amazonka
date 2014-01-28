{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your queues.
module Network.AWS.SQS.ListQueues where

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

data ListQueues = ListQueues
    { lqrQueueNamePrefix :: Maybe Text
      -- ^ A string to use for filtering the list results. Only those queues whose
      -- name begins with the specified string are returned.
    } deriving (Eq, Show, Generic)

instance ToQuery ListQueues

instance AWSRequest ListQueues where
    type Er ListQueues = SQSError
    type Rs ListQueues = ListQueuesResponse
    request = getQuery service "ListQueues"

data ListQueuesResponse = ListQueuesResponse
    { lqrrsQueueUrls :: [Text]
      -- ^ A list of queue URLs, up to 1000 entries.
    } deriving (Eq, Show, Generic)

instance FromXML ListQueuesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListQueuesResponse"
        :| ["ListQueuesResult"]
