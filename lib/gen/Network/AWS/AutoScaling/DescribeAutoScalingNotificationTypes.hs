{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all notification types that are supported by Auto
-- Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes where

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

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeAutoScalingNotificationTypes

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Er DescribeAutoScalingNotificationTypes = AutoScalingError
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse
    request = getQuery service "DescribeAutoScalingNotificationTypes"

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { dasntaAutoScalingNotificationTypes :: [Text]
      -- ^ Returns a list of all notification types supported by Auto Scaling. They
      -- are: autoscaling:EC2_INSTANCE_LAUNCH autoscaling:EC2_INSTANCE_LAUNCH_ERROR
      -- autoscaling:EC2_INSTANCE_TERMINATE autoscaling:EC2_INSTANCE_TERMINATE_ERROR
      -- autoscaling:TEST_NOTIFICATION
      -- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeAutoScalingNotificationTypes
      -- &AUTHPARAMS autoscaling:EC2_INSTANCE_LAUNCH
      -- autoscaling:EC2_INSTANCE_LAUNCH_ERROR autoscaling:EC2_INSTANCE_TERMINATE
      -- autoscaling:EC2_INSTANCE_TERMINATE_ERROR autoscaling:TEST_NOTIFICATION
      -- 42fc6794-bf21-11e2-a1cf-ff3dEXAMPLE.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAutoScalingNotificationTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAutoScalingNotificationTypesResponse"
        :| ["DescribeAutoScalingNotificationTypesResult"]
