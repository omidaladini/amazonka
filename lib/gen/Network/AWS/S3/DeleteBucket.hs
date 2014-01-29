{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
module Network.AWS.S3.DeleteBucket where

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
deleteBucket :: Text
             -> AWS (Either S3Error DeleteBucketResponse)
deleteBucket p1 = undefined $ DeleteBucket
    { dbBucket = p1
    }

data DeleteBucket = DeleteBucket
    { dbBucket :: !Text
    } deriving (Generic)

instance ToHeaders DeleteBucket

instance ToPath DeleteBucket where
    toPath DeleteBucket{..} = Text.concat
        [ "/"
        , toText dbBucket
        ]

instance ToQuery DeleteBucket where
    toQuery = const mempty

instance AWSRequest DeleteBucket where
    type Er DeleteBucket = S3Error
    type Rs DeleteBucket = DeleteBucketResponse
    request  = deleteS3 service
    response = undefined

data DeleteBucketResponse = DeleteBucketResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteBucketResponse where
    fromXMLOptions = xmlOptions
