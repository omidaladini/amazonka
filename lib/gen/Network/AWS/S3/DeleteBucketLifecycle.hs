{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the lifecycle configuration from the bucket.
module Network.AWS.S3.DeleteBucketLifecycle where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteBucketLifecycle :: Text
                      -> DeleteBucketLifecycle
deleteBucketLifecycle p1 = DeleteBucketLifecycle
    { dblBucket = p1
    }

data DeleteBucketLifecycle = DeleteBucketLifecycle
    { dblBucket :: !Text
    } deriving (Generic)

instance ToHeaders DeleteBucketLifecycle

instance ToPath DeleteBucketLifecycle where
    toPath DeleteBucketLifecycle{..} = Text.concat
        [ "/"
        , toText dblBucket
        ]

instance ToQuery DeleteBucketLifecycle where
    toQuery DeleteBucketLifecycle{..} = queryFromList
        [ "lifecycle"
        ]

instance AWSRequest DeleteBucketLifecycle where
    type Er DeleteBucketLifecycle = S3Error
    type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse
    request  = deleteS3 service
    response = undefined

data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteBucketLifecycleResponse where
    fromXMLOptions = xmlOptions
