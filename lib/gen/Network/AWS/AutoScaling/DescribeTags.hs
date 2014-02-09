{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the Auto Scaling group tags. You can use filters to limit results
-- when describing tags. For example, you can query for tags of a particular
-- Auto Scaling group. You can specify multiple values for a filter. A tag
-- must match at least one of the specified values for it to be included in
-- the results. You can also specify multiple filters. The result includes
-- information for a particular tag only if it matches all your filters. If
-- there's no match, no special message is returned.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeTags
-- &AUTHPARAMS my-test-asg true 1.0 version auto-scaling-group
-- 086265fd-bf3e-11e2-85fc-fbb1EXAMPLE.
module Network.AWS.AutoScaling.DescribeTags where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeTags = DescribeTags
    { dttFilters :: [Filter]
      -- ^ The value of the filter type used to identify the tags to be returned. For
      -- example, you can filter so that tags are returned according to Auto Scaling
      -- group, the key and value, or whether the new tag will be applied to
      -- instances launched after the tag is created (PropagateAtLaunch).
    , dttMaxRecords :: Maybe Int
      -- ^ The maximum number of records to return.
    , dttNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeTags

instance AWSRequest DescribeTags where
    type Er DescribeTags = AutoScalingError
    type Rs DescribeTags = DescribeTagsResponse
    request = getQuery service "DescribeTags"

instance AWSPager DescribeTags where
    next rq rs
        | Just x <- dttrNextToken rs = Just $ rq { dttNextToken = Just x }
        | otherwise = Nothing

data DescribeTagsResponse = DescribeTagsResponse
    { dttrNextToken :: Maybe Text
      -- ^ A string used to mark the start of the next batch of returned results.
    , dttrTags :: [TagDescription]
      -- ^ The list of tags.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeTagsResponse"
        :| ["DescribeTagsResult"]
