{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudTrail.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudTrail.Service

-- | The settings for a trail.
data Trail = Trail
    { tIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Set to True to include AWS API calls from AWS global services such as IAM.
      -- Otherwise, False.
    , tName :: Maybe Text
      -- ^ Name of the trail set by calling CreateTrail.
    , tS3BucketName :: Maybe Text
      -- ^ Name of the Amazon S3 bucket into which CloudTrail delivers your trail
      -- files.
    , tS3KeyPrefix :: Maybe Text
      -- ^ Value of the Amazon S3 prefix.
    , tSnsTopicName :: Maybe Text
      -- ^ Name of the existing Amazon SNS topic that CloudTrail uses to notify the
      -- account owner when new CloudTrail log files have been delivered.
    } deriving (Eq, Show, Generic)

instance FromJSON Trail
instance ToJSON Trail
