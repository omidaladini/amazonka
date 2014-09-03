{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the tags from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketTagging
    (
    -- * Request
      DeleteBucketTagging
    -- ** Request constructor
    , deleteBucketTagging
    -- ** Request lenses
    , dbtrBucket

    -- * Response
    , DeleteBucketTaggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteBucketTagging' request.
deleteBucketTagging :: BucketName -- ^ 'dbtrBucket'
                    -> DeleteBucketTagging
deleteBucketTagging p1 = DeleteBucketTagging
    { _dbtrBucket = p1
    }

data DeleteBucketTagging = DeleteBucketTagging
    { _dbtrBucket :: BucketName
    } deriving (Show, Generic)

dbtrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> DeleteBucketTagging
    -> f DeleteBucketTagging
dbtrBucket f x =
    (\y -> x { _dbtrBucket = y })
       <$> f (_dbtrBucket x)
{-# INLINE dbtrBucket #-}

instance ToPath DeleteBucketTagging where
    toPath DeleteBucketTagging{..} = mconcat
        [ "/"
        , toBS _dbtrBucket
        ]

instance ToQuery DeleteBucketTagging where
    toQuery DeleteBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders DeleteBucketTagging

instance ToBody DeleteBucketTagging

data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = DeleteBucketTaggingResponse

    request = delete
    response _ = nullaryResponse DeleteBucketTaggingResponse
