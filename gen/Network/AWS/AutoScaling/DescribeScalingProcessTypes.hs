{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns scaling process types for use in the ResumeProcesses and
-- SuspendProcesses actions.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeScalingProcessTypes &AUTHPARAMS AZRebalance
-- AddToLoadBalancer AlarmNotification HealthCheck Launch ReplaceUnhealthy
-- ScheduledActions Terminate 27f2eacc-b73f-11e2-ad99-c7aba3a9c963.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeScalingProcessTypes = DescribeScalingProcessTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeScalingProcessTypes

instance AWSRequest DescribeScalingProcessTypes where
    type Er DescribeScalingProcessTypes = AutoScalingError
    type Rs DescribeScalingProcessTypes = DescribeScalingProcessTypesResponse
    request  = postQuery service "DescribeScalingProcessTypes"
    response = responseXML

data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { ptProcesses :: [ProcessType]
      -- ^ A list of ProcessType names.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeScalingProcessTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeScalingProcessTypesResponse"
        :| ["DescribeScalingProcessTypesResult"]
