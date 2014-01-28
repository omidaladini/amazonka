{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the versioning state of a bucket.
module Network.AWS.S3.GetBucketVersioning where

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

data GetBucketVersioning = GetBucketVersioning
    { gbvBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketVersioning

instance ToPath GetBucketVersioning where
    toPath GetBucketVersioning{..} = Text.concat
        [ "/"
        , toText gbvBucket
        ]

instance ToQuery GetBucketVersioning where
    toQuery GetBucketVersioning{..} = List
        [ "versioning"
        ]

instance AWSRequest GetBucketVersioning where
    type Er GetBucketVersioning = S3Error
    type Rs GetBucketVersioning = GetBucketVersioningResponse
    request  = getS3 service
    response = undefined

data GetBucketVersioningResponse = GetBucketVersioningResponse
    { gbvrsMfaDelete :: Maybe MfaDelete
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has been
      -- configured with MFA delete. If the bucket has never been so configured,
      -- this element is not returned.
    , gbvrsStatus :: Maybe Status
      -- ^ The versioning state of the bucket.
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketVersioningResponse where
    fromXMLOptions = xmlOptions

getBucketVersioning :: Text -- ^ Bucket
                    -> AWS (Either S3Error GetBucketVersioningResponse)
getBucketVersioning p1 = undefined $ GetBucketVersioning
    { gbvBucket = p1
    }
