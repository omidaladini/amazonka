{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the cors configuration for the bucket.
module Network.AWS.S3.GetBucketCors where

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

data GetBucketCors = GetBucketCors
    { gbcBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketCors

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = Text.concat
        [ "/"
        , toText gbcBucket
        ]

instance ToQuery GetBucketCors where
    toQuery GetBucketCors{..} = List
        [ "cors"
        ]

instance AWSRequest GetBucketCors where
    type Er GetBucketCors = S3Error
    type Rs GetBucketCors = GetBucketCorsResponse
    request  = getS3 service
    response = undefined

data GetBucketCorsResponse = GetBucketCorsResponse
    { gbcrsCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketCorsResponse where
    fromXMLOptions = xmlOptions

-- | Convenience method utilising default fields where applicable.
getBucketCors :: Text -- ^ Bucket
              -> AWS (Either S3Error GetBucketCorsResponse)
getBucketCors p1 = undefined $ GetBucketCors
    { gbcBucket = p1
    }
