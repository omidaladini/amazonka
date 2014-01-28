{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the tags for a bucket.
module Network.AWS.S3.PutBucketTagging where

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

data PutBucketTagging = PutBucketTagging
    { pbtBucket :: !Text
    , pbtContentMD5 :: Maybe Text
    , pbtTagging :: Tagging
    } deriving (Generic)

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} =
        [ "Content-MD5" =: pbtContentMD5
        ]

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = Text.concat
        [ "/"
        , toText pbtBucket
        ]

instance ToQuery PutBucketTagging where
    toQuery PutBucketTagging{..} = List
        [ "tagging"
        ]

instance AWSRequest PutBucketTagging where
    type Er PutBucketTagging = S3Error
    type Rs PutBucketTagging = PutBucketTaggingResponse
    request  = putS3 service
    response = undefined

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketTaggingResponse where
    fromXMLOptions = xmlOptions

putBucketTagging :: Text -- ^ Bucket
                 -> Tagging -- ^ Tagging
                 -> AWS (Either S3Error PutBucketTaggingResponse)
putBucketTagging p1 p2 = undefined $ PutBucketTagging
    { pbtBucket = p1
    , pbtTagging = p2
    , pbtContentMD5 = Nothing
    }
