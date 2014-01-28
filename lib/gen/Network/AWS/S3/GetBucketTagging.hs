{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.GetBucketTagging where

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

data GetBucketTagging = GetBucketTagging
    { gbtBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketTagging

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = Text.concat
        [ "/"
        , toText gbtBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery GetBucketTagging{..} = List
        [ "tagging"
        ]

instance AWSRequest GetBucketTagging where
    type Er GetBucketTagging = S3Error
    type Rs GetBucketTagging = GetBucketTaggingResponse
    request  = getS3 service
    response = undefined

data GetBucketTaggingResponse = GetBucketTaggingResponse
    { gbtrsTagSet :: [Tag]
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketTaggingResponse where
    fromXMLOptions = xmlOptions

-- | Convenience method utilising default fields where applicable.
getBucketTagging :: Text -- ^ Bucket
                 -> AWS (Either S3Error GetBucketTaggingResponse)
getBucketTagging p1 = undefined $ GetBucketTagging
    { gbtBucket = p1
    }
