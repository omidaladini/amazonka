{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SetQueueAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the value of one or more queue attributes. Valid attributes that can
-- be set are [VisibilityTimeout, Policy, MaximumMessageSize,
-- MessageRetentionPeriod, ReceiveMessageWaitTimeSeconds].
module Network.AWS.SQS.SetQueueAttributes where

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
setQueueAttributes :: HashMap QueueAttributeName Text
                   -> Text
                   -> SetQueueAttributes
setQueueAttributes p1 p2 = undefined $ SetQueueAttributes
    { sqarAttributes = p1
    , sqarQueueUrl = p2
    }

data SetQueueAttributes = SetQueueAttributes
    { sqarAttributes :: HashMap QueueAttributeName Text
      -- ^ A map of attributes to set.
    , sqarQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery SetQueueAttributes

instance AWSRequest SetQueueAttributes where
    type Er SetQueueAttributes = SQSError
    type Rs SetQueueAttributes = SetQueueAttributesResponse
    request = getQuery service "SetQueueAttributes"

data SetQueueAttributesResponse = SetQueueAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML SetQueueAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetQueueAttributesResponse"
        :| ["SetQueueAttributesResult"]
