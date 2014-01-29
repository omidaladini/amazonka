{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Network.AWS.Redshift.EnableLogging where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
enableLogging :: Text
              -> Text
              -> EnableLogging
enableLogging p1 p2 = undefined $ EnableLogging
    { elmBucketName = p1
    , elmClusterIdentifier = p2
    , elmS3KeyPrefix = Nothing
    }

data EnableLogging = EnableLogging
    { elmBucketName :: !Text
      -- ^ The name of an existing S3 bucket where the log files are to be stored.
      -- Constraints: Must be in the same region as the cluster The cluster must
      -- have read bucket and put object permissions.
    , elmClusterIdentifier :: !Text
      -- ^ The identifier of the cluster on which logging is to be started. Example:
      -- examplecluster.
    , elmS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names. Constraints: Cannot exceed 512
      -- characters Cannot contain spaces( ), double quotes ("), single quotes ('),
      -- a backslash (\), or control characters. The hexadecimal codes for invalid
      -- characters are: x00 to x20 x22 x27 x5c x7f or larger.
    } deriving (Eq, Show, Generic)

instance ToQuery EnableLogging

instance AWSRequest EnableLogging where
    type Er EnableLogging = RedshiftError
    type Rs EnableLogging = EnableLoggingResponse
    request = getQuery service "EnableLogging"

data EnableLoggingResponse = EnableLoggingResponse
    { elmrsBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , elmrsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , elmrsLastFailureTime :: Maybe UTCTime
      -- ^ The last time when logs failed to be delivered.
    , elmrsLastSuccessfulDeliveryTime :: Maybe UTCTime
      -- ^ The last time when logs were delivered.
    , elmrsLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , elmrsS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    } deriving (Eq, Show, Generic)

instance FromXML EnableLoggingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "EnableLoggingResponse"
        :| ["EnableLoggingResult"]
