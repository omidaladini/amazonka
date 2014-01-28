{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
module Network.AWS.S3.PutBucketVersioning where

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

data PutBucketVersioning = PutBucketVersioning
    { pbvBucket :: !Text
    , pbvContentMD5 :: Maybe Text
    , pbvMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a space,
      -- and the value that is displayed on your authentication device.
    , pbvVersioningConfiguration :: VersioningConfiguration
    } deriving (Generic)

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} =
        [ "Content-MD5" =: pbvContentMD5
        , "x-amz-mfa" =: pbvMFA
        ]

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = Text.concat
        [ "/"
        , toText pbvBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery PutBucketVersioning{..} = List
        [ "versioning"
        ]

instance AWSRequest PutBucketVersioning where
    type Er PutBucketVersioning = S3Error
    type Rs PutBucketVersioning = PutBucketVersioningResponse
    request  = putS3 service
    response = undefined

data PutBucketVersioningResponse = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketVersioningResponse where
    fromXMLOptions = xmlOptions

putBucketVersioning :: Text -- ^ Bucket
                    -> VersioningConfiguration -- ^ VersioningConfiguration
                    -> AWS (Either S3Error PutBucketVersioningResponse)
putBucketVersioning p1 p2 = undefined $ PutBucketVersioning
    { pbvBucket = p1
    , pbvVersioningConfiguration = p2
    , pbvContentMD5 = Nothing
    , pbvMFA = Nothing
    }
