{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
module Network.AWS.CloudWatch.DisableAlarmActions where

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
disableAlarmActions :: [Text]
                    -> DisableAlarmActions
disableAlarmActions p1 = DisableAlarmActions
    { daaiAlarmNames = p1
    }

data DisableAlarmActions = DisableAlarmActions
    { daaiAlarmNames :: [Text]
      -- ^ The names of the alarms to disable actions for.
    } deriving (Eq, Show, Generic)

instance ToQuery DisableAlarmActions

instance AWSRequest DisableAlarmActions where
    type Er DisableAlarmActions = CloudWatchError
    type Rs DisableAlarmActions = DisableAlarmActionsResponse
    request = getQuery service "DisableAlarmActions"

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Show, Generic)

instance FromXML DisableAlarmActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DisableAlarmActionsResponse
