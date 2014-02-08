{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Executes the specified policy.
module Network.AWS.AutoScaling.ExecutePolicy where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
executePolicy :: ResourceName
              -- ^ The name or ARN of the policy you want to run.
              -> ExecutePolicy
executePolicy p1 = ExecutePolicy
    { eptPolicyName = p1
    , eptAutoScalingGroupName = Nothing
    , eptHonorCooldown = Nothing
    }

data ExecutePolicy = ExecutePolicy
    { eptAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name or the Amazon Resource Name (ARN) of the Auto Scaling group.
    , eptHonorCooldown :: Maybe Bool
      -- ^ Set to True if you want Auto Scaling to wait for the cooldown period
      -- associated with the Auto Scaling group to complete before executing the
      -- policy. Set to False if you want Auto Scaling to circumvent the cooldown
      -- period associated with the Auto Scaling group and execute the policy before
      -- the cooldown period ends. For information about cooldown period, see
      -- Cooldown Period in the Auto Scaling Developer Guide.
    , eptPolicyName :: !ResourceName
      -- ^ The name or ARN of the policy you want to run.
    } deriving (Eq, Show, Generic)

instance ToQuery ExecutePolicy

instance AWSRequest ExecutePolicy where
    type Er ExecutePolicy = AutoScalingError
    type Rs ExecutePolicy = ExecutePolicyResponse
    request = getQuery service "ExecutePolicy"

data ExecutePolicyResponse = ExecutePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML ExecutePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExecutePolicyResponse"
