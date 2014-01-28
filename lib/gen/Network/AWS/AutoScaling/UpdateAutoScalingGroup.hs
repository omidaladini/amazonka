{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the configuration for the specified AutoScalingGroup. To update an
-- Auto Scaling group with a launch configuration that has the
-- InstanceMonitoring flag set to False, you must first ensure that collection
-- of group metrics is disabled. Otherwise, calls to UpdateAutoScalingGroup
-- will fail. If you have previously enabled group metrics collection, you can
-- disable collection of all group metrics by calling
-- DisableMetricsCollection. The new settings are registered upon the
-- completion of this call. Any launch configuration settings take effect on
-- any triggers after this call returns. Scaling activities that are currently
-- in progress aren't affected. If a new value is specified for MinSize
-- without specifying the value for DesiredCapacity, and if the new MinSize is
-- larger than the current size of the Auto Scaling Group, there will be an
-- implicit call to SetDesiredCapacity to set the group to the new MinSize. If
-- a new value is specified for MaxSize without specifying the value for
-- DesiredCapacity, and the new MaxSize is smaller than the current size of
-- the Auto Scaling Group, there will be an implicit call to
-- SetDesiredCapacity to set the group to the new MaxSize. All other optional
-- parameters are left unchanged if not passed in the request. Update existing
-- Auto Scaling group with ELB health check
-- https://autoscaling.amazonaws.com/?HealthCheckType=ELB
-- &HealthCheckGracePeriod=300 &AutoScalingGroupName=my-test-asg-lbs
-- &Version=2011-01-01 &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE Update existing Auto Scaling group
-- with a new Availability Zone
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg-lbs
-- &AvailabilityZones.member.1=us-east-1a
-- &AvailabilityZones.member.2=us-east-1b
-- &AvailabilityZones.member.3=us-east-1c &MinSize=3 &Version=2011-01-01
-- &Action=UpdateAutoScalingGroup &AUTHPARAMS
-- adafead0-ab8a-11e2-ba13-ab0ccEXAMPLE.
module Network.AWS.AutoScaling.UpdateAutoScalingGroup where

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

data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { uasgtAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group.
    , uasgtAvailabilityZones :: [Text]
      -- ^ Availability Zones for the group.
    , uasgtDefaultCooldown :: Maybe Int
      -- ^ The amount of time, in seconds, after a scaling activity completes before
      -- any further scaling activities can start. For more information, see
      -- Cooldown Period.
    , uasgtDesiredCapacity :: Maybe Int
      -- ^ The desired capacity for the Auto Scaling group.
    , uasgtHealthCheckGracePeriod :: Maybe Int
      -- ^ The length of time that Auto Scaling waits before checking an instance's
      -- health status. The grace period begins when an instance comes into service.
    , uasgtHealthCheckType :: Maybe Text
      -- ^ The type of health check for the instances in the Auto Scaling group. The
      -- health check type can either be EC2 for Amazon EC2 or ELB for Elastic Load
      -- Balancing.
    , uasgtLaunchConfigurationName :: Maybe ResourceName
      -- ^ The name of the launch configuration.
    , uasgtMaxSize :: Maybe Int
      -- ^ The maximum size of the Auto Scaling group.
    , uasgtMinSize :: Maybe Int
      -- ^ The minimum size of the Auto Scaling group.
    , uasgtPlacementGroup :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
    , uasgtTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies used to
      -- select the instance to terminate. The policies are executed in the order
      -- that they are listed. For more information on creating a termination policy
      -- for your Auto Scaling group, go to Instance Termination Policy for Your
      -- Auto Scaling Group in the the Auto Scaling Developer Guide.
    , uasgtvpcZoneIdentifier :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if applicable. You can
      -- specify several subnets in a comma-separated list. When you specify
      -- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
      -- Availability Zones match the values you specify for AvailabilityZones. For
      -- more information on creating your Auto Scaling group in Amazon VPC by
      -- specifying subnets, see Launch Auto Scaling Instances into Amazon VPC in
      -- the the Auto Scaling Developer Guide.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateAutoScalingGroup

instance AWSRequest UpdateAutoScalingGroup where
    type Er UpdateAutoScalingGroup = AutoScalingError
    type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse
    request = getQuery service "UpdateAutoScalingGroup"

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateAutoScalingGroupResponse"
        :| ["UpdateAutoScalingGroupResult"]
