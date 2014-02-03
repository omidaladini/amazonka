{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables actions for the specified alarms.
module Network.AWS.CloudWatch.EnableAlarmActions where

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
enableAlarmActions :: [Text]
                   -> EnableAlarmActions
enableAlarmActions p1 = EnableAlarmActions
    { eaaiAlarmNames = p1
    }

data EnableAlarmActions = EnableAlarmActions
    { eaaiAlarmNames :: [Text]
      -- ^ The names of the alarms to enable actions for.
    } deriving (Eq, Show, Generic)

instance ToQuery EnableAlarmActions

instance AWSRequest EnableAlarmActions where
    type Er EnableAlarmActions = CloudWatchError
    type Rs EnableAlarmActions = EnableAlarmActionsResponse
    request = getQuery service "EnableAlarmActions"

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Show, Generic)

instance FromXML EnableAlarmActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot EnableAlarmActionsResponse
