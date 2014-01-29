{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
module Network.AWS.S3.PutBucketWebsite where

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
putBucketWebsite :: Text
                 -> WebsiteConfiguration
                 -> AWS (Either S3Error PutBucketWebsiteResponse)
putBucketWebsite p1 p2 = undefined $ PutBucketWebsite
    { pbwBucket = p1
    , pbwWebsiteConfiguration = p2
    , pbwContentMD5 = Nothing
    }

data PutBucketWebsite = PutBucketWebsite
    { pbwBucket :: !Text
    , pbwContentMD5 :: Maybe Text
    , pbwWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Generic)

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} =
        [ "Content-MD5" =: pbwContentMD5
        ]

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = Text.concat
        [ "/"
        , toText pbwBucket
        ]

instance ToQuery PutBucketWebsite where
    toQuery PutBucketWebsite{..} = List
        [ "website"
        ]

instance AWSRequest PutBucketWebsite where
    type Er PutBucketWebsite = S3Error
    type Rs PutBucketWebsite = PutBucketWebsiteResponse
    request  = putS3 service
    response = undefined

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketWebsiteResponse where
    fromXMLOptions = xmlOptions
