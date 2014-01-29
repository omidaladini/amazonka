{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes whether information, such as queries and connection attempts, is
-- being logged for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DescribeLoggingStatus where

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
describeLoggingStatus :: Text
                      -> DescribeLoggingStatus
describeLoggingStatus p1 = undefined $ DescribeLoggingStatus
    { dlsmClusterIdentifier = p1
    }

data DescribeLoggingStatus = DescribeLoggingStatus
    { dlsmClusterIdentifier :: !Text
      -- ^ The identifier of the cluster to get the logging status from. Example:
      -- examplecluster.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeLoggingStatus

instance AWSRequest DescribeLoggingStatus where
    type Er DescribeLoggingStatus = RedshiftError
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse
    request = getQuery service "DescribeLoggingStatus"

data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { dlsmrsBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , dlsmrsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , dlsmrsLastFailureTime :: Maybe UTCTime
      -- ^ The last time when logs failed to be delivered.
    , dlsmrsLastSuccessfulDeliveryTime :: Maybe UTCTime
      -- ^ The last time when logs were delivered.
    , dlsmrsLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , dlsmrsS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeLoggingStatusResponse"
        :| ["DescribeLoggingStatusResult"]
