{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
-- Support for passing Trail as a parameter ends as early as February 25,
-- 2014. The request and response examples in this topic show the use of
-- parameters as well as a Trail object. Until Trail is removed, you can use
-- either Trail or the parameter list.
module Network.AWS.CloudTrail.CreateTrail where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createTrail :: CreateTrail
createTrail = CreateTrail
    { ctrIncludeGlobalServiceEvents = Nothing
    , ctrName = Nothing
    , ctrS3BucketName = Nothing
    , ctrS3KeyPrefix = Nothing
    , ctrSnsTopicName = Nothing
    , ctrTrail = Nothing
    }

data CreateTrail = CreateTrail
    { ctrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global services such
      -- as IAM to the log files.
    , ctrName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , ctrS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for publishing log
      -- files.
    , ctrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the bucket you
      -- have designated for log file delivery.
    , ctrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for notification of log
      -- file delivery.
    , ctrTrail :: Maybe Trail
      -- ^ Support for passing a Trail object in the CreateTrail or UpdateTrail
      -- actions will end as early as February 15, 2014. Instead of the Trail object
      -- and its members, use the parameters listed for these actions.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateTrail

instance AWSRequest CreateTrail where
    type Er CreateTrail = CloudTrailError
    type Rs CreateTrail = CreateTrailResponse
    request  = getJSON service
    response = responseJSON

data CreateTrailResponse = CreateTrailResponse
    { ctrrsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global services such
      -- as IAM to the log files.
    , ctrrsName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , ctrrsS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for publishing log
      -- files.
    , ctrrsS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the bucket you
      -- have designated for log file delivery.
    , ctrrsSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for notification of log
      -- file delivery.
    , ctrrsTrail :: Maybe Trail
      -- ^ Support for passing a Trail object in the CreateTrail or UpdateTrail
      -- actions will end as early as February 15, 2014. Instead of the Trail object
      -- and its members, use the parameters listed for these actions.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateTrailResponse
