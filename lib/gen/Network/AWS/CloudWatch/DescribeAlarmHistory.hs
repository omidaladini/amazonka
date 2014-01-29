{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms. Amazon CloudWatch retains the
-- history of an alarm for two weeks, whether or not you delete the alarm.
module Network.AWS.CloudWatch.DescribeAlarmHistory where

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
describeAlarmHistory :: AWS (Either CloudWatchError DescribeAlarmHistoryResponse)
describeAlarmHistory = undefined $ DescribeAlarmHistory
    { dahiAlarmName = Nothing
    , dahiEndDate = Nothing
    , dahiHistoryItemType = Nothing
    , dahiMaxRecords = Nothing
    , dahiNextToken = Nothing
    , dahiStartDate = Nothing
    }

data DescribeAlarmHistory = DescribeAlarmHistory
    { dahiAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , dahiEndDate :: Maybe UTCTime
      -- ^ The ending date to retrieve alarm history.
    , dahiHistoryItemType :: Maybe HistoryItemType
      -- ^ The type of alarm histories to retrieve.
    , dahiMaxRecords :: Maybe Int
      -- ^ The maximum number of alarm history records to retrieve.
    , dahiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is more data
      -- available.
    , dahiStartDate :: Maybe UTCTime
      -- ^ The starting date to retrieve alarm history.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAlarmHistory

instance AWSRequest DescribeAlarmHistory where
    type Er DescribeAlarmHistory = CloudWatchError
    type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse
    request = getQuery service "DescribeAlarmHistory"

instance AWSPager DescribeAlarmHistory where
    next rq rs
        | Just x <- dahirsNextToken rs = Just $ rq { dahiNextToken = Just x }
        | otherwise = Nothing

data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { dahirsAlarmHistoryItems :: [AlarmHistoryItem]
      -- ^ A list of alarm histories in JSON format.
    , dahirsNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAlarmHistoryResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAlarmHistoryResponse"
        :| ["DescribeAlarmHistoryResult"]
