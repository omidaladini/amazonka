{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return the notification configuration of a bucket.
module Network.AWS.S3.GetBucketNotification where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketNotification :: Text
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { gbnBucket = p1
    }

data GetBucketNotification = GetBucketNotification
    { gbnBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketNotification

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = Text.concat
        [ "/"
        , toText gbnBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery GetBucketNotification{..} = queryFromList
        [ "notification"
        ]

instance AWSRequest GetBucketNotification where
    type Er GetBucketNotification = S3Error
    type Rs GetBucketNotification = GetBucketNotificationResponse
    request  = getS3 service
    response = undefined

data GetBucketNotificationResponse = GetBucketNotificationResponse
    { gbnrsTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions
