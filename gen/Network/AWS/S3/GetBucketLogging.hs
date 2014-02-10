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

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketLogging = GetBucketLogging
    { gbleBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = Text.concat
        [ "/"
        , toText gbleBucket
        ]

instance ToQuery GetBucketLogging where
    toQuery GetBucketLogging{..} = queryFromList
        [ "logging"
        ]

instance ToHeaders GetBucketLogging

instance AWSRequest GetBucketLogging where
    type Er GetBucketLogging = S3Error
    type Rs GetBucketLogging = GetBucketLoggingResponse
    request rq = s3 GET (service $ gbleBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketLoggingResponse
        <$> xml "LoggingEnabled" doc

data GetBucketLoggingResponse = GetBucketLoggingResponse
    { gblerLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show)
