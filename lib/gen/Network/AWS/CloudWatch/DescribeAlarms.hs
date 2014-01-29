{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
module Network.AWS.CloudWatch.DescribeAlarms where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

-- | Convenience method utilising default fields where applicable.
describeAlarms :: AWS (Either CloudWatchError DescribeAlarmsResponse)
describeAlarms = undefined $ DescribeAlarms
    { daiActionPrefix = Nothing
    , daiAlarmNamePrefix = Nothing
    , daiAlarmNames = []
    , daiMaxRecords = Nothing
    , daiNextToken = Nothing
    , daiStateValue = Nothing
    }

data DescribeAlarms = DescribeAlarms
    { daiActionPrefix :: Maybe Text
      -- ^ The action name prefix.
    , daiAlarmNamePrefix :: Maybe Text
      -- ^ The alarm name prefix. AlarmNames cannot be specified if this parameter is
      -- specified.
    , daiAlarmNames :: [Text]
      -- ^ A list of alarm names to retrieve information for.
    , daiMaxRecords :: Maybe Int
      -- ^ The maximum number of alarm descriptions to retrieve.
    , daiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is more data
      -- available.
    , daiStateValue :: Maybe StateValue
      -- ^ The state value to be used in matching alarms.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAlarms

instance AWSRequest DescribeAlarms where
    type Er DescribeAlarms = CloudWatchError
    type Rs DescribeAlarms = DescribeAlarmsResponse
    request = getQuery service "DescribeAlarms"

instance AWSPager DescribeAlarms where
    next rq rs
        | Just x <- dairsNextToken rs = Just $ rq { daiNextToken = Just x }
        | otherwise = Nothing

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { dairsMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for the specified alarms.
    , dairsNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAlarmsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAlarmsResponse"
        :| ["DescribeAlarmsResult"]
