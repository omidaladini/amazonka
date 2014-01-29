{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.ResumeProcesses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resumes all suspended Auto Scaling processes for an Auto Scaling group. For
-- information on suspending and resuming Auto Scaling process, see Suspend
-- and Resume Auto Scaling Process.
module Network.AWS.AutoScaling.ResumeProcesses where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
resumeProcesses :: ResourceName
                -> ResumeProcesses
resumeProcesses p1 = undefined $ ResumeProcesses
    { sprAutoScalingGroupName = p1
    , sprScalingProcesses = []
    }

data ResumeProcesses = ResumeProcesses
    { sprAutoScalingGroupName :: !ResourceName
      -- ^ The name or Amazon Resource Name (ARN) of the Auto Scaling group.
    , sprScalingProcesses :: [Text]
      -- ^ The processes that you want to suspend or resume, which can include one or
      -- more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
      -- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To suspend
      -- all process types, omit this parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery ResumeProcesses

instance AWSRequest ResumeProcesses where
    type Er ResumeProcesses = AutoScalingError
    type Rs ResumeProcesses = ResumeProcessesResponse
    request = getQuery service "ResumeProcesses"

data ResumeProcessesResponse = ResumeProcessesResponse
    deriving (Eq, Show, Generic)

instance FromXML ResumeProcessesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ResumeProcessesResponse"
        :| ["ResumeProcessesResult"]
