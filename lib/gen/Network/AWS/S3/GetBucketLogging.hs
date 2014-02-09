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

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketLogging :: Text
                 -> GetBucketLogging
getBucketLogging p1 = GetBucketLogging
    { gboBucket = p1
    }

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
    toQuery GetBucketLogging{..} = queryFromList
        [ "logging"
        ]

instance AWSRequest GetBucketLogging where
    type Er GetBucketLogging = S3Error
    type Rs GetBucketLogging = GetBucketLoggingResponse
    request  = getS3 service
    response = undefined

data GetBucketLoggingResponse = GetBucketLoggingResponse
    { gborLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketLoggingResponse where
    fromXMLOptions = xmlOptions
