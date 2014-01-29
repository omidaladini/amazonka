{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ChangeMessageVisibility action changes the visibility timeout of a
-- specified message in a queue to a new value. The maximum allowed timeout
-- value you can set the value to is 12 hours. This means you can't extend the
-- timeout of a message in an existing queue to more than a total visibility
-- timeout of 12 hours. (For more information visibility timeout, see
-- Visibility Timeout in the Amazon SQS Developer Guide.) For example, let's
-- say you have a message and its default message visibility timeout is 30
-- minutes. You could call ChangeMessageVisiblity with a value of two hours
-- and the effective timeout would be two hours and 30 minutes. When that time
-- comes near you could again extend the time out by calling
-- ChangeMessageVisiblity, but this time the maximum allowed timeout would be
-- 9 hours and 30 minutes. If you attempt to set the VisibilityTimeout to an
-- amount more than the maximum time left, Amazon SQS returns an error. It
-- will not automatically recalculate and increase the timeout to the maximum
-- time remaining. Unlike with a queue, when you change the visibility timeout
-- for a specific message, that timeout value is applied immediately but is
-- not saved in memory for that message. If you don't delete a message after
-- it is received, the visibility timeout for the message the next time it is
-- received reverts to the original timeout value, not the value you set with
-- the ChangeMessageVisibility action.
module Network.AWS.SQS.ChangeMessageVisibility where

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
changeMessageVisibility :: Text
                        -> Text
                        -> Int
                        -> ChangeMessageVisibility
changeMessageVisibility p1 p2 p3 = undefined $ ChangeMessageVisibility
    { cmvrQueueUrl = p1
    , cmvrReceiptHandle = p2
    , cmvrVisibilityTimeout = p3
    }

data ChangeMessageVisibility = ChangeMessageVisibility
    { cmvrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    , cmvrReceiptHandle :: !Text
      -- ^ The receipt handle associated with the message whose visibility timeout
      -- should be changed.
    , cmvrVisibilityTimeout :: !Int
      -- ^ The new value (in seconds) for the message's visibility timeout.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeMessageVisibility

instance AWSRequest ChangeMessageVisibility where
    type Er ChangeMessageVisibility = SQSError
    type Rs ChangeMessageVisibility = ChangeMessageVisibilityResponse
    request = getQuery service "ChangeMessageVisibility"

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse
    deriving (Eq, Show, Generic)

instance FromXML ChangeMessageVisibilityResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ChangeMessageVisibilityResponse"
        :| ["ChangeMessageVisibilityResult"]
