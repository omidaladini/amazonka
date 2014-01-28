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

data PutBucketLogging = PutBucketLogging
    { pblBucket :: !Text
    , pblBucketLoggingStatus :: BucketLoggingStatus
    , pblContentMD5 :: Maybe Text
    } deriving (Generic)

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} =
        [ "Content-MD5" =: pblContentMD5
        ]

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = Text.concat
        [ "/"
        , toText pblBucket
        ]

instance ToQuery PutBucketLogging where
    toQuery PutBucketLogging{..} = List
        [ "logging"
        ]

instance AWSRequest PutBucketLogging where
    type Er PutBucketLogging = S3Error
    type Rs PutBucketLogging = PutBucketLoggingResponse
    request  = putS3 service
    response = undefined

data PutBucketLoggingResponse = PutBucketLoggingResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketLoggingResponse where
    fromXMLOptions = xmlOptions

-- | Convenience method utilising default fields where applicable.
putBucketLogging :: Text -- ^ Bucket
                 -> BucketLoggingStatus -- ^ BucketLoggingStatus
                 -> AWS (Either S3Error PutBucketLoggingResponse)
putBucketLogging p1 p2 = undefined $ PutBucketLogging
    { pblBucket = p1
    , pblBucketLoggingStatus = p2
    , pblContentMD5 = Nothing
    }
