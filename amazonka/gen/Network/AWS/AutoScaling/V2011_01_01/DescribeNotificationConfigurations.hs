{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
module Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeNotificationConfigurations' request.
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames = mempty
    , _dncuMaxRecords = Nothing
    , _dncuNextToken = Nothing
    }

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames :: [Text]
      -- ^ The name of the Auto Scaling group.
    , _dncuMaxRecords :: Maybe Integer
      -- ^ Maximum number of records to be returned.
    , _dncuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Show, Generic)

makeLenses ''DescribeNotificationConfigurations

instance ToQuery DescribeNotificationConfigurations where
    toQuery = genericToQuery def

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { _dncaNotificationConfigurations :: [NotificationConfiguration]
      -- ^ The list of notification configurations.
    , _dncaNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Show, Generic)

makeLenses ''DescribeNotificationConfigurationsResponse

instance AWSRequest DescribeNotificationConfigurations where
    type Sv DescribeNotificationConfigurations = AutoScaling
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse

    request = post "DescribeNotificationConfigurations"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeNotificationConfigurationsResponse
            <*> xml %| "NotificationConfigurations"
            <*> xml %|? "XmlString"

instance AWSPager DescribeNotificationConfigurations where
    next rq rs = (\x -> rq { _dncuNextToken = Just x })
        <$> (_dncaNextToken rs)