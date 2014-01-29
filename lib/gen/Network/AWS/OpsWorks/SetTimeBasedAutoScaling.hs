{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specify the time-based auto scaling configuration for a specified instance.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. Required Permissions: To use this action, an IAM user must have
-- a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields where applicable.
setTimeBasedAutoScaling :: Text
                        -> AWS (Either OpsWorksError SetTimeBasedAutoScalingResponse)
setTimeBasedAutoScaling p1 = undefined $ SetTimeBasedAutoScaling
    { stbasrInstanceId = p1
    , stbasrAutoScalingSchedule = Nothing
    }

data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling
    { stbasrAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
      -- ^ An AutoScalingSchedule with the instance schedule.
    , stbasrInstanceId :: !Text
      -- ^ The instance ID.
    } deriving (Eq, Show, Generic)

instance ToJSON SetTimeBasedAutoScaling

instance AWSRequest SetTimeBasedAutoScaling where
    type Er SetTimeBasedAutoScaling = OpsWorksError
    type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse
    request  = getJSON service
    response = responseJSON

data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetTimeBasedAutoScalingResponse
