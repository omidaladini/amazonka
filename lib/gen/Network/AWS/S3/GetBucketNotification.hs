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

-- | Convenience method utilising default fields where applicable.
getBucketNotification :: Text -- ^ Bucket
                      -> AWS (Either S3Error GetBucketNotificationResponse)
getBucketNotification p1 = undefined $ GetBucketNotification
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
    toQuery GetBucketNotification{..} = List
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
