{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    (
    -- * Request
      DescribeAutoScalingNotificationTypes
    -- ** Request constructor
    , mkDescribeAutoScalingNotificationTypes
    -- * Response
    , DescribeAutoScalingNotificationTypesResponse
    -- ** Response constructor
    , mkDescribeAutoScalingNotificationTypesResponse
    -- ** Response lenses
    , dasntrAutoScalingNotificationTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingNotificationTypes' request.
mkDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes
mkDescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes

instance ToQuery DescribeAutoScalingNotificationTypes where
    toQuery = genericQuery def

-- | The AutoScalingNotificationTypes data type.
newtype DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntrAutoScalingNotificationTypes :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingNotificationTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingNotificationTypes ::@ @[Text]@
--
mkDescribeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse
mkDescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntrAutoScalingNotificationTypes = mempty
    }

-- | Returns a list of all notification types supported by Auto Scaling. They
-- are: autoscaling:EC2_INSTANCE_LAUNCH autoscaling:EC2_INSTANCE_LAUNCH_ERROR
-- autoscaling:EC2_INSTANCE_TERMINATE autoscaling:EC2_INSTANCE_TERMINATE_ERROR
-- autoscaling:TEST_NOTIFICATION
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeAutoScalingNotificationTypes
-- &AUTHPARAMS autoscaling:EC2_INSTANCE_LAUNCH
-- autoscaling:EC2_INSTANCE_LAUNCH_ERROR autoscaling:EC2_INSTANCE_TERMINATE
-- autoscaling:EC2_INSTANCE_TERMINATE_ERROR autoscaling:TEST_NOTIFICATION
-- 42fc6794-bf21-11e2-a1cf-ff3dEXAMPLE.
dasntrAutoScalingNotificationTypes :: Lens' DescribeAutoScalingNotificationTypesResponse [Text]
dasntrAutoScalingNotificationTypes =
    lens _dasntrAutoScalingNotificationTypes
         (\s a -> s { _dasntrAutoScalingNotificationTypes = a })

instance FromXML DescribeAutoScalingNotificationTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Sv DescribeAutoScalingNotificationTypes = AutoScaling
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse

    request = post "DescribeAutoScalingNotificationTypes"
    response _ = xmlResponse