{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all termination policies supported by Auto Scaling.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeTerminationPolicyTypes &AUTHPARAMS
-- ClosestToNextInstanceHour Default NewestInstance OldestInstance
-- OldestLaunchConfiguration d9a05827-b735-11e2-a40c-c79a5EXAMPLE.
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeTerminationPolicyTypes

instance AWSRequest DescribeTerminationPolicyTypes where
    type Er DescribeTerminationPolicyTypes = AutoScalingError
    type Rs DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypesResponse
    request  = postQuery service "DescribeTerminationPolicyTypes"
    response = responseXML

data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { dtptaTerminationPolicyTypes :: [Text]
      -- ^ Termination policies supported by Auto Scaling. They are: OldestInstance,
      -- OldestLaunchConfiguration, NewestInstance, ClosestToNextInstanceHour,
      -- Default.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeTerminationPolicyTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeTerminationPolicyTypesResponse"
        :| ["DescribeTerminationPolicyTypesResult"]