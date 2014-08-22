{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the Spot Price history. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide. When you specify an Availability Zone,
-- this operation describes the price history for the specified Availability
-- Zone with the most recent set of prices listed first. If you don't specify
-- an Availability Zone, you get the prices across all Availability Zones,
-- starting with the most recent set. However, if you're using an API version
-- earlier than 2011-05-15, you get the lowest price across the region for the
-- specified time period. The prices returned are listed in chronological
-- order, from the oldest to the most recent. Example This example gets Spot
-- Price history for a particular day in December 2009 for the specified
-- Availability Zone.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;StartTime=2009-12-04T00:00:00.000Z
-- &amp;EndTime=2009-12-04T23:59:59.000Z &amp;AvailabilityZone=us-east-1a
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE m1.small Linux/UNIX
-- 0.287 2009-12-04T20:56:05.000Z us-east-1a m1.small Windows 0.033
-- 2009-12-04T22:33:47.000Z us-east-1a Example with Filters This example uses
-- filters to get the same results as the previous example.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;Filter.1.Name=timestamp &amp;Filter.1.Value.1=2009-12-04*
-- &amp;Filter.2.Name=availability-zone &amp;Filter.2.Value.1=us-east-1a
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSpotPriceHistory' request.
describeSpotPriceHistory :: DescribeSpotPriceHistory
describeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphrDryRun = Nothing
    , _dsphrStartTime = Nothing
    , _dsphrEndTime = Nothing
    , _dsphrFilters = mempty
    , _dsphrInstanceTypes = mempty
    , _dsphrMaxResults = Nothing
    , _dsphrProductDescriptions = mempty
    , _dsphrNextToken = Nothing
    , _dsphrAvailabilityZone = Nothing
    }

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphrDryRun :: Maybe Bool
      -- ^ 
    , _dsphrStartTime :: Maybe ISO8601
      -- ^ The start date and time of the Spot Price history data.
    , _dsphrEndTime :: Maybe ISO8601
      -- ^ The end date and time of the Spot Price history data.
    , _dsphrFilters :: [Filter]
      -- ^ One or more filters. availability-zone - The Availability Zone
      -- for which prices should be returned. instance-type - The type of
      -- instance (for example, m1.small). product-description - The
      -- product description for the Spot Price (Linux/UNIX | SUSE Linux |
      -- Windows | Linux/UNIX (Amazon VPC) | SUSE Linux (Amazon VPC) |
      -- Windows (Amazon VPC)). spot-price - The Spot Price. The value
      -- must match exactly (or use wildcards; greater than or less than
      -- comparison is not supported). timestamp - The timestamp of the
      -- Spot Price history (for example, 2010-08-16T05:06:11.000Z). You
      -- can use wildcards (* and ?). Greater than or less than comparison
      -- is not supported.
    , _dsphrInstanceTypes :: [InstanceType]
      -- ^ One or more instance types.
    , _dsphrMaxResults :: Maybe Integer
      -- ^ The number of rows to return.
    , _dsphrProductDescriptions :: [Text]
      -- ^ One or more basic product descriptions.
    , _dsphrNextToken :: Maybe Text
      -- ^ The next set of rows to return.
    , _dsphrAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    } deriving (Show, Generic)

makeLenses ''DescribeSpotPriceHistory

instance ToQuery DescribeSpotPriceHistory where
    toQuery = genericQuery def

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphsSpotPriceHistory :: [SpotPrice]
      -- ^ The historical Spot Prices.
    , _dsphsNextToken :: Maybe Text
      -- ^ The string marking the next set of results. This is empty if
      -- there are no more results.
    } deriving (Show, Generic)

makeLenses ''DescribeSpotPriceHistoryResponse

instance FromXML DescribeSpotPriceHistoryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSpotPriceHistory where
    type Sv DescribeSpotPriceHistory = EC2
    type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse

    request = post "DescribeSpotPriceHistory"
    response _ = xmlResponse

instance AWSPager DescribeSpotPriceHistory where
    next rq rs = (\x -> rq { _dsphrNextToken = Just x })
        <$> (_dsphsNextToken rs)