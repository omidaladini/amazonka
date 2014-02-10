{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeBundleTasks operation describes in-progress and recent bundle
-- tasks. Complete and failed tasks are removed from the list a short time
-- after completion. If no bundle ids are given, all bundle tasks are
-- returned.
module Network.AWS.EC2.DescribeBundleTasks where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeBundleTasks = DescribeBundleTasks
    { dbtBundleIds :: [Text]
      -- ^ The list of bundle task IDs to describe.
    , dbtDryRun :: Maybe Bool
    , dbtFilters :: [Filter]
      -- ^ A list of filters used to match properties for BundleTasks. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeBundleTasks

instance AWSRequest DescribeBundleTasks where
    type Er DescribeBundleTasks = EC2Error
    type Rs DescribeBundleTasks = DescribeBundleTasksResponse
    request  = postQuery service "DescribeBundleTasks"
    response = responseXML

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { dbtrBundleInstanceTasksSet :: [BundleTask]
      -- ^ The list of described bundle tasks.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeBundleTasksResponse where
    fromXMLOptions = xmlOptions