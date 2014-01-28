{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops logging information, such as queries and connection attempts, for the
-- specified Amazon Redshift cluster.
module Network.AWS.Redshift.DisableLogging where

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

data DisableLogging = DisableLogging
    { dlmClusterIdentifier :: !Text
      -- ^ The identifier of the cluster on which logging is to be stopped. Example:
      -- examplecluster.
    } deriving (Eq, Show, Generic)

instance ToQuery DisableLogging

instance AWSRequest DisableLogging where
    type Er DisableLogging = RedshiftError
    type Rs DisableLogging = DisableLoggingResponse
    request = getQuery service "DisableLogging"

data DisableLoggingResponse = DisableLoggingResponse
    { dlmrsBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , dlmrsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , dlmrsLastFailureTime :: Maybe UTCTime
      -- ^ The last time when logs failed to be delivered.
    , dlmrsLastSuccessfulDeliveryTime :: Maybe UTCTime
      -- ^ The last time when logs were delivered.
    , dlmrsLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , dlmrsS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    } deriving (Eq, Show, Generic)

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DisableLoggingResponse"
        :| ["DisableLoggingResult"]
