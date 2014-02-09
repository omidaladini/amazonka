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

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketNotification = GetBucketNotification
    { gbnBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = Text.concat
        [ "/"
        , toText gbnBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery GetBucketNotification{..} = queryFromList
        [ "notification"
        ]

instance ToHeaders GetBucketNotification

instance AWSRequest GetBucketNotification where
    type Er GetBucketNotification = S3Error
    type Rs GetBucketNotification = GetBucketNotificationResponse
    request rq = s3 GET (service $ gbnBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketNotificationResponse
        <$> xml "TopicConfiguration" doc

data GetBucketNotificationResponse = GetBucketNotificationResponse
    { gbnrTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Show)
