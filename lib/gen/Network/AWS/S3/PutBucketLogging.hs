{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
module Network.AWS.S3.PutBucketLogging where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketLogging :: Text
                 -> BucketLoggingStatus
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { pblBucket = p1
    , pblBucketLoggingStatus = p2
    , pblContentMD5 = Nothing
    }

data PutBucketLogging = PutBucketLogging
    { pblBucket :: !Text
    , pblBucketLoggingStatus :: BucketLoggingStatus
    , pblContentMD5 :: Maybe Text
    } deriving (Generic)

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = Text.concat
        [ "/"
        , toText pblBucket
        ]

instance ToQuery PutBucketLogging where
    toQuery PutBucketLogging{..} = queryFromList
        [ "logging"
        ]

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} =
        [ "Content-MD5" =: pblContentMD5
        ]

instance AWSRequest PutBucketLogging where
    type Er PutBucketLogging = S3Error
    type Rs PutBucketLogging = PutBucketLoggingResponse
    request rq = s3XML PUT (service $ pblBucket rq) (pblBucketLoggingStatus rq) rq
    response = receiveEmpty PutBucketLoggingResponse

data PutBucketLoggingResponse = PutBucketLoggingResponse
   deriving (Eq, Show)