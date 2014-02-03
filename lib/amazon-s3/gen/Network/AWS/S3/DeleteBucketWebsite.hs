{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the website configuration from the bucket.
module Network.AWS.S3.DeleteBucketWebsite where

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
deleteBucketWebsite :: Text
                    -> DeleteBucketWebsite
deleteBucketWebsite p1 = DeleteBucketWebsite
    { dbwBucket = p1
    }

data DeleteBucketWebsite = DeleteBucketWebsite
    { dbwBucket :: !Text
    } deriving (Generic)

instance ToHeaders DeleteBucketWebsite

instance ToPath DeleteBucketWebsite where
    toPath DeleteBucketWebsite{..} = Text.concat
        [ "/"
        , toText dbwBucket
        ]

instance ToQuery DeleteBucketWebsite where
    toQuery DeleteBucketWebsite{..} = queryFromList
        [ "website"
        ]

instance AWSRequest DeleteBucketWebsite where
    type Er DeleteBucketWebsite = S3Error
    type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse
    request  = deleteS3 service
    response = undefined

data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteBucketWebsiteResponse where
    fromXMLOptions = xmlOptions
