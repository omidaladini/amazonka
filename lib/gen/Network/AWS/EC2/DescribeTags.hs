{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the tags for the specified resources.
module Network.AWS.EC2.DescribeTags where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeTags = DescribeTags
    { dtrDryRun :: Maybe Bool
    , dtrFilters :: [Filter]
      -- ^ A list of filters used to match properties for tags.
    , dtrMaxResults :: Maybe Int
    , dtrNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeTags

instance AWSRequest DescribeTags where
    type Er DescribeTags = EC2Error
    type Rs DescribeTags = DescribeTagsResponse
    request = getQuery service "DescribeTags"

instance AWSPager DescribeTags where
    next rq rs
        | Just x <- dtrrNextToken rs = Just $ rq { dtrNextToken = Just x }
        | otherwise = Nothing

data DescribeTagsResponse = DescribeTagsResponse
    { dtrrNextToken :: Maybe Text
    , dtrrTags :: [TagDescription]
      -- ^ A list of the tags for the specified resources.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions
