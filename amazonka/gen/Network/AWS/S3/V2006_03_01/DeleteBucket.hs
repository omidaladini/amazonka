{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucket
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
module Network.AWS.S3.V2006_03_01.DeleteBucket where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)


-- | Default DeleteBucket request.
deleteBucket :: BucketName -- ^ 'dbrBucket'
             -> DeleteBucket
deleteBucket p1 = DeleteBucket
    { dbrBucket = p1
    }

data DeleteBucket = DeleteBucket
    { dbrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath DeleteBucket where
    toPath DeleteBucket{..} = mconcat
        [ "/"
        , toBS dbrBucket
        ]

instance ToQuery DeleteBucket

instance ToHeaders DeleteBucket

instance ToBody DeleteBucket

instance AWSRequest DeleteBucket where
    type Sv DeleteBucket = S3

    request  = delete
    response = headerResposne $ const DeleteBucketResponse

data instance Rs DeleteBucket = DeleteBucketResponse
    deriving (Eq, Show, Generic)