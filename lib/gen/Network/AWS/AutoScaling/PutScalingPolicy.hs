{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameter(s) you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScalingAdjustment=30 &AdjustmentType=PercentChangeInCapacity
-- &PolicyName=my-scaleout-policy &Version=2011-01-01 &Action=PutScalingPolicy
-- &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:b0dcf5e8
-- -02e6-4e31-9719-0675d0dc31ae:autoScalingGroupName/my-test-asg:policyName/my-scal
-- eout-policy 3cfc6fef-c08b-11e2-a697-2922EXAMPLE.
module Network.AWS.AutoScaling.PutScalingPolicy where

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
putScalingPolicy :: Text
                 -> ResourceName
                 -> Text
                 -> Int
                 -> PutScalingPolicy
putScalingPolicy p1 p2 p3 p4 = undefined $ PutScalingPolicy
    { psptAdjustmentType = p1
    , psptAutoScalingGroupName = p2
    , psptPolicyName = p3
    , psptScalingAdjustment = p4
    , psptCooldown = Nothing
    , psptMinAdjustmentStep = Nothing
    }

data PutScalingPolicy = PutScalingPolicy
    { psptAdjustmentType :: !Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or a
      -- percentage of the current capacity. Valid values are ChangeInCapacity,
      -- ExactCapacity, and PercentChangeInCapacity. For more information about the
      -- adjustment types supported by Auto Scaling, see Scale Based on Demand.
    , psptAutoScalingGroupName :: !ResourceName
      -- ^ The name or ARN of the Auto Scaling group.
    , psptCooldown :: Maybe Int
      -- ^ The amount of time, in seconds, after a scaling activity completes and
      -- before the next scaling activity can start. For more information, see
      -- Cooldown Period.
    , psptMinAdjustmentStep :: Maybe Int
      -- ^ Used with AdjustmentType with the value PercentChangeInCapacity, the
      -- scaling policy changes the DesiredCapacity of the Auto Scaling group by at
      -- least the number of instances specified in the value. You will get a
      -- ValidationError if you use MinAdjustmentStep on a policy with an
      -- AdjustmentType other than PercentChangeInCapacity.
    , psptPolicyName :: !Text
      -- ^ The name of the policy you want to create or update.
    , psptScalingAdjustment :: !Int
      -- ^ The number of instances by which to scale. AdjustmentType determines the
      -- interpretation of this number (e.g., as an absolute number or as a
      -- percentage of the existing Auto Scaling group size). A positive increment
      -- adds to the current capacity and a negative value removes from the current
      -- capacity.
    } deriving (Eq, Show, Generic)

instance ToQuery PutScalingPolicy

instance AWSRequest PutScalingPolicy where
    type Er PutScalingPolicy = AutoScalingError
    type Rs PutScalingPolicy = PutScalingPolicyResponse
    request = getQuery service "PutScalingPolicy"

data PutScalingPolicyResponse = PutScalingPolicyResponse
    { psptrsPolicyARN :: Maybe ResourceName
      -- ^ A policy's Amazon Resource Name (ARN).
    } deriving (Eq, Show, Generic)

instance FromXML PutScalingPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PutScalingPolicyResponse"
        :| ["PutScalingPolicyResult"]
