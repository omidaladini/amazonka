{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the cors configuration for a bucket.
module Network.AWS.S3.PutBucketCors where

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

data PutBucketCors = PutBucketCors
    { pbcBucket :: !Text
    , pbcCORSConfiguration :: Maybe CORSConfiguration
    , pbcContentMD5 :: Maybe Text
    } deriving (Generic)

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} =
        [ "Content-MD5" =: pbcContentMD5
        ]

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = Text.concat
        [ "/"
        , toText pbcBucket
        ]

instance ToQuery PutBucketCors where
    toQuery PutBucketCors{..} = List
        [ "cors"
        ]

instance AWSRequest PutBucketCors where
    type Er PutBucketCors = S3Error
    type Rs PutBucketCors = PutBucketCorsResponse
    request  = putS3 service
    response = undefined

data PutBucketCorsResponse = PutBucketCorsResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketCorsResponse where
    fromXMLOptions = xmlOptions

putBucketCors :: Text -- ^ Bucket
              -> AWS (Either S3Error PutBucketCorsResponse)
putBucketCors p1 = undefined $ PutBucketCors
    { pbcBucket = p1
    , pbcCORSConfiguration = Nothing
    , pbcContentMD5 = Nothing
    }
