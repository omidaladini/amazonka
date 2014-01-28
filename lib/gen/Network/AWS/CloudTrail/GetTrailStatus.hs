{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail. The CloudTrail API
-- is currently undergoing revision. This action currently returns both new
-- fields and fields slated for removal from the API. The following lists
-- indicate the plans for each field: List of Members Planned for Ongoing
-- Support IsLogging LatestDeliveryTime LatestNotificationTime
-- StartLoggingTime StopLoggingTime LatestNotificationError
-- LatestDeliveryError List of Members Scheduled for Removal
-- LatestDeliveryAttemptTime: Use LatestDeliveryTime instead.
-- LatestNotificationAttemptTime: Use LatestNotificationTime instead.
-- LatestDeliveryAttemptSucceeded: No replacement. See the note following this
-- list. LatestNotificationAttemptSucceeded: No replacement. See the note
-- following this list. TimeLoggingStarted: Use StartLoggingTime instead.
-- TimeLoggingStopped: Use StopLoggingtime instead. No replacements have been
-- created for LatestDeliveryAttemptSucceeded and
-- LatestNotificationAttemptSucceeded. Use LatestDeliveryError and
-- LatestNotificationError to evaluate success or failure of log delivery or
-- notification. Empty values returned for these fields indicate success. An
-- error in LatestDeliveryError generally indicates either a missing bucket or
-- insufficient permissions to write to the bucket. Similarly, an error in
-- LatestNotificationError indicates either a missing topic or insufficient
-- permissions.
module Network.AWS.CloudTrail.GetTrailStatus where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudTrail.Service
import Network.AWS.CloudTrail.Types

data GetTrailStatus = GetTrailStatus
    { gtsrName :: !Text
      -- ^ The name of the trail for which you are requesting the current status.
    } deriving (Eq, Show, Generic)

instance ToJSON GetTrailStatus

instance AWSRequest GetTrailStatus where
    type Er GetTrailStatus = CloudTrailError
    type Rs GetTrailStatus = GetTrailStatusResponse
    request  = getJSON service
    response = responseJSON

data GetTrailStatusResponse = GetTrailStatusResponse
    { gtsrrsIsLogging :: Maybe Bool
      -- ^ Whether the CloudTrail is currently logging AWS API calls.
    , gtsrrsLatestDeliveryAttemptSucceeded :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , gtsrrsLatestDeliveryAttemptTime :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , gtsrrsLatestDeliveryError :: Maybe Text
      -- ^ Displays any Amazon S3 error that CloudTrail encountered when attempting to
      -- deliver log files to the designated bucket. For more information see the
      -- topic Error Responses in the Amazon S3 API Reference.
    , gtsrrsLatestDeliveryTime :: Maybe UTCTime
      -- ^ Specifies the date and time that CloudTrail last delivered log files to an
      -- account's Amazon S3 bucket.
    , gtsrrsLatestNotificationAttemptSucceeded :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , gtsrrsLatestNotificationAttemptTime :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , gtsrrsLatestNotificationError :: Maybe Text
      -- ^ Displays any Amazon SNS error that CloudTrail encountered when attempting
      -- to send a notification. For more information about Amazon SNS errors, see
      -- the Amazon SNS Developer Guide.
    , gtsrrsLatestNotificationTime :: Maybe UTCTime
      -- ^ Specifies the date and time of the most recent Amazon SNS notification that
      -- CloudTrail has written a new log file to an account's Amazon S3 bucket.
    , gtsrrsStartLoggingTime :: Maybe UTCTime
      -- ^ Specifies the most recent date and time when CloudTrail started recording
      -- API calls for an AWS account.
    , gtsrrsStopLoggingTime :: Maybe UTCTime
      -- ^ Specifies the most recent date and time when CloudTrail stopped recording
      -- API calls for an AWS account.
    , gtsrrsTimeLoggingStarted :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , gtsrrsTimeLoggingStopped :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    } deriving (Eq, Show, Generic)

instance FromJSON GetTrailStatusResponse
