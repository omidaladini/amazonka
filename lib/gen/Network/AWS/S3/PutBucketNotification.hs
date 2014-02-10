{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables notifications of specified events for a bucket.
module Network.AWS.S3.PutBucketNotification where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketNotification :: Text
                      -> NotificationConfiguration
                      -> PutBucketNotification
putBucketNotification p1 p2 = PutBucketNotification
    { pbnBucket = p1
    , pbnNotificationConfiguration = p2
    , pbnContentMD5 = Nothing
    }

data PutBucketNotification = PutBucketNotification
    { pbnBucket :: !Text
    , pbnContentMD5 :: Maybe Text
    , pbnNotificationConfiguration :: NotificationConfiguration
    } deriving (Generic)

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = Text.concat
        [ "/"
        , toText pbnBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery PutBucketNotification{..} = queryFromList
        [ "notification"
        ]

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} =
        [ "Content-MD5" =: pbnContentMD5
        ]

instance AWSRequest PutBucketNotification where
    type Er PutBucketNotification = S3Error
    type Rs PutBucketNotification = PutBucketNotificationResponse
    request rq = s3XML PUT (service $ pbnBucket rq) (pbnNotificationConfiguration rq) rq
    response = receiveEmpty PutBucketNotificationResponse

data PutBucketNotificationResponse = PutBucketNotificationResponse
   deriving (Eq, Show)
