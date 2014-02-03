{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
module Network.AWS.CloudWatch.SetAlarmState where

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

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
setAlarmState :: Text
              -> Text
              -> StateValue
              -> SetAlarmState
setAlarmState p1 p2 p3 = SetAlarmState
    { sasiAlarmName = p1
    , sasiStateReason = p2
    , sasiStateValue = p3
    , sasiStateReasonData = Nothing
    }

data SetAlarmState = SetAlarmState
    { sasiAlarmName :: !Text
      -- ^ The descriptive name for the alarm. This name must be unique within the
      -- user's AWS account. The maximum length is 255 characters.
    , sasiStateReason :: !Text
      -- ^ The reason that this alarm is set to this specific state (in human-readable
      -- text format).
    , sasiStateReasonData :: Maybe Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- machine-readable JSON format).
    , sasiStateValue :: !StateValue
      -- ^ The value of the state.
    } deriving (Eq, Show, Generic)

instance ToQuery SetAlarmState

instance AWSRequest SetAlarmState where
    type Er SetAlarmState = CloudWatchError
    type Rs SetAlarmState = SetAlarmStateResponse
    request = getQuery service "SetAlarmState"

data SetAlarmStateResponse = SetAlarmStateResponse
    deriving (Eq, Show, Generic)

instance FromXML SetAlarmStateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot SetAlarmStateResponse
