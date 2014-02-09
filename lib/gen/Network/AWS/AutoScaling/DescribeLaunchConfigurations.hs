{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of the launch configurations, or the specified
-- launch configurations, if they exist. If no name is specified, then the
-- full details of all launch configurations are returned.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationNames.member.1=my-test-lc
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeLaunchConfigurations
-- &AUTHPARAMS true 2013-01-21T23:04:42.200Z my-test-lc m1.small
-- arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:
-- 9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc
-- ami-514ac838 true false d05a22f8-b690-11e2-bf8e-2113fEXAMPLE.
module Network.AWS.AutoScaling.DescribeLaunchConfigurations where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { lcnuLaunchConfigurationNames :: [ResourceName]
      -- ^ A list of launch configuration names.
    , lcnuMaxRecords :: Maybe Int
      -- ^ The maximum number of launch configurations. The default is 100.
    , lcnuNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeLaunchConfigurations

instance AWSRequest DescribeLaunchConfigurations where
    type Er DescribeLaunchConfigurations = AutoScalingError
    type Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse
    request = getQuery service "DescribeLaunchConfigurations"

instance AWSPager DescribeLaunchConfigurations where
    next rq rs
        | Just x <- lcnurNextToken rs = Just $ rq { lcnuNextToken = Just x }
        | otherwise = Nothing

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { lcnurLaunchConfigurations :: [LaunchConfiguration]
      -- ^ A list of launch configurations.
    , lcnurNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeLaunchConfigurationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeLaunchConfigurationsResponse"
        :| ["DescribeLaunchConfigurationsResult"]
