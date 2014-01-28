{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
module Network.AWS.S3.GetBucketLogging where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.Conduit
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.S3.Service
import Network.AWS.S3.Types

data GetBucketLogging = GetBucketLogging
    { gboBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketLogging

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = Text.concat
        [ "/"
        , toText gboBucket
        ]

instance ToQuery GetBucketLogging where
    toQuery GetBucketLogging{..} = List
        [ "logging"
        ]

instance AWSRequest GetBucketLogging where
    type Er GetBucketLogging = S3Error
    type Rs GetBucketLogging = GetBucketLoggingResponse
    request  = getS3 service
    response = undefined

data GetBucketLoggingResponse = GetBucketLoggingResponse
    { gborsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketLoggingResponse where
    fromXMLOptions = xmlOptions
