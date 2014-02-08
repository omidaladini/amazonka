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

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteBucket :: Text
             -> DeleteBucket
deleteBucket p1 = DeleteBucket
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
