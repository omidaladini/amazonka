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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketNotification :: Text
                      -> NotificationConfiguration
                      -> PutBucketNotification
putBucketNotification p1 p2 = undefined $ PutBucketNotification
    { pbnBucket = p1
    , pbnNotificationConfiguration = p2
    , pbnContentMD5 = Nothing
    }

data PutBucketNotification = PutBucketNotification
    { pbnBucket :: !Text
    , pbnContentMD5 :: Maybe Text
    , pbnNotificationConfiguration :: NotificationConfiguration
    } deriving (Generic)

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} =
        [ "Content-MD5" =: pbnContentMD5
        ]

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = Text.concat
        [ "/"
        , toText pbnBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery PutBucketNotification{..} = List
        [ "notification"
        ]

instance AWSRequest PutBucketNotification where
    type Er PutBucketNotification = S3Error
    type Rs PutBucketNotification = PutBucketNotificationResponse
    request  = putS3 service
    response = undefined

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketNotificationResponse where
    fromXMLOptions = xmlOptions
