{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.EnableLogging
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
module Network.AWS.Redshift.V2012_12_01.EnableLogging where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EnableLogging' request.
enableLogging :: Text -- ^ '_elmClusterIdentifier'
              -> Text -- ^ '_elmBucketName'
              -> EnableLogging
enableLogging p1 p2 = EnableLogging
    { _elmClusterIdentifier = p1
    , _elmBucketName = p2
    , _elmS3KeyPrefix = Nothing
    }

data EnableLogging = EnableLogging
    { _elmClusterIdentifier :: Text
      -- ^ The identifier of the cluster on which logging is to be started.
      -- Example: examplecluster.
    , _elmBucketName :: Text
      -- ^ The name of an existing S3 bucket where the log files are to be
      -- stored. Constraints: Must be in the same region as the cluster
      -- The cluster must have read bucket and put object permissions.
    , _elmS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names. Constraints: Cannot
      -- exceed 512 characters Cannot contain spaces( ), double quotes
      -- ("), single quotes ('), a backslash (\), or control characters.
      -- The hexadecimal codes for invalid characters are: x00 to x20 x22
      -- x27 x5c x7f or larger.
    } deriving (Show, Generic)

makeLenses ''EnableLogging

instance ToQuery EnableLogging where
    toQuery = genericToQuery def

data EnableLoggingResponse = EnableLoggingResponse
    { _ltLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _ltS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _ltBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _ltLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , _ltLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _ltLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    } deriving (Show, Generic)

makeLenses ''EnableLoggingResponse

instance AWSRequest EnableLogging where
    type Sv EnableLogging = Redshift
    type Rs EnableLogging = EnableLoggingResponse

    request = post "EnableLogging"
    response _ = cursorResponse $ \hs xml ->
        pure EnableLoggingResponse
            <*> xml %|? "Boolean"
            <*> xml %|? "String"
            <*> xml %|? "String"
            <*> xml %|? "String"
            <*> xml %|? "TStamp"
            <*> xml %|? "TStamp"