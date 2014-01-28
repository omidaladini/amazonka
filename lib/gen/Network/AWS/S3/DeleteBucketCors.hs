{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the cors configuration information set for the bucket.
module Network.AWS.S3.DeleteBucketCors where

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

data DeleteBucketCors = DeleteBucketCors
    { dbcBucket :: !Text
    } deriving (Generic)

instance ToHeaders DeleteBucketCors

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = Text.concat
        [ "/"
        , toText dbcBucket
        ]

instance ToQuery DeleteBucketCors where
    toQuery DeleteBucketCors{..} = List
        [ "cors"
        ]

instance AWSRequest DeleteBucketCors where
    type Er DeleteBucketCors = S3Error
    type Rs DeleteBucketCors = DeleteBucketCorsResponse
    request  = deleteS3 service
    response = undefined

data DeleteBucketCorsResponse = DeleteBucketCorsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteBucketCorsResponse where
    fromXMLOptions = xmlOptions

-- | Convenience method utilising default fields where applicable.
deleteBucketCors :: Text -- ^ Bucket
                 -> AWS (Either S3Error DeleteBucketCorsResponse)
deleteBucketCors p1 = undefined $ DeleteBucketCors
    { dbcBucket = p1
    }
