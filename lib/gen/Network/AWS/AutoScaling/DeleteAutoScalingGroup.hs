{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Auto Scaling group if the group has no instances and
-- no scaling activities in progress. To remove all instances before calling
-- DeleteAutoScalingGroup, you can call UpdateAutoScalingGroup to set the
-- minimum and maximum size of the AutoScalingGroup to zero.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ForceDelete=true &Version=2011-01-01 &Action=DeleteAutoScalingGroup
-- &AUTHPARAMS 70a76d42-9665-11e2-9fdf-211deEXAMPLE.
module Network.AWS.AutoScaling.DeleteAutoScalingGroup where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteAutoScalingGroup :: ResourceName
                       -> DeleteAutoScalingGroup
deleteAutoScalingGroup p1 = DeleteAutoScalingGroup
    { dasgtAutoScalingGroupName = p1
    , dasgtForceDelete = Nothing
    }

data DeleteAutoScalingGroup = DeleteAutoScalingGroup
    { dasgtAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group to delete.
    , dasgtForceDelete :: Maybe Bool
      -- ^ Starting with API version 2011-01-01, specifies that the Auto Scaling group
      -- will be deleted along with all instances associated with the group, without
      -- waiting for all instances to be terminated.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteAutoScalingGroup

instance AWSRequest DeleteAutoScalingGroup where
    type Er DeleteAutoScalingGroup = AutoScalingError
    type Rs DeleteAutoScalingGroup = DeleteAutoScalingGroupResponse
    request = getQuery service "DeleteAutoScalingGroup"

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAutoScalingGroupResponse"
