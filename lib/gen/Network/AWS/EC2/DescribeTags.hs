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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeTags = DescribeTags
    { dtrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dtrFilters :: [Filter]
      -- ^ A list of filters used to match properties for tags.
    , dtrMaxResults :: Maybe Int
      -- ^ FIXME: Missing documentation
    , dtrNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeTags

instance AWSRequest DescribeTags where
    type Er DescribeTags = EC2Error
    type Rs DescribeTags = DescribeTagsResponse
    request = v2Query service GET "DescribeTags"

instance AWSPager DescribeTags where
    next rq rs
        | Just x <- dtrrsNextToken rs = Just $ rq { dtrNextToken = Just x }
        | otherwise = Nothing

data DescribeTagsResponse = DescribeTagsResponse
    { dtrrsNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dtrrsTags :: [TagDescription]
      -- ^ A list of the tags for the specified resources.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions
