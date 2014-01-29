{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.GetQueueUrl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetQueueUrl action returns the URL of an existing queue.
module Network.AWS.SQS.GetQueueUrl where

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
getQueueUrl :: Text
            -> AWS (Either SQSError GetQueueUrlResponse)
getQueueUrl p1 = undefined $ GetQueueUrl
    { gqurQueueName = p1
    , gqurQueueOwnerAWSAccountId = Nothing
    }

data GetQueueUrl = GetQueueUrl
    { gqurQueueName :: !Text
      -- ^ The name of the queue whose URL must be fetched.
    , gqurQueueOwnerAWSAccountId :: Maybe Text
      -- ^ The AWS account number of the queue's owner.
    } deriving (Eq, Show, Generic)

instance ToQuery GetQueueUrl

instance AWSRequest GetQueueUrl where
    type Er GetQueueUrl = SQSError
    type Rs GetQueueUrl = GetQueueUrlResponse
    request = getQuery service "GetQueueUrl"

data GetQueueUrlResponse = GetQueueUrlResponse
    { gqurrsQueueUrl :: Maybe Text
      -- ^ The URL for the queue.
    } deriving (Eq, Show, Generic)

instance FromXML GetQueueUrlResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetQueueUrlResponse"
        :| ["GetQueueUrlResult"]
