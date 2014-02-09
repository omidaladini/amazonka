{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeConversionTasks
module Network.AWS.EC2.DescribeConversionTasks where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeConversionTasks = DescribeConversionTasks
    { dctrConversionTaskIds :: [Text]
    , dctrDryRun :: Maybe Bool
    , dctrFilters :: [Filter]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeConversionTasks

instance AWSRequest DescribeConversionTasks where
    type Er DescribeConversionTasks = EC2Error
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse
    request = getQuery service "DescribeConversionTasks"

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { dctrrConversionTasks :: [ConversionTask]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeConversionTasksResponse where
    fromXMLOptions = xmlOptions
