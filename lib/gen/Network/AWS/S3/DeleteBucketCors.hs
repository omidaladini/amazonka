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

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data DeleteBucketCors = DeleteBucketCors
    { dbcBucket :: !Text
    } deriving (Generic)

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = Text.concat
        [ "/"
        , toText dbcBucket
        ]

instance ToQuery DeleteBucketCors where
    toQuery DeleteBucketCors{..} = queryFromList
        [ "cors"
        ]

instance ToHeaders DeleteBucketCors

instance AWSRequest DeleteBucketCors where
    type Er DeleteBucketCors = S3Error
    type Rs DeleteBucketCors = DeleteBucketCorsResponse
    request rq = s3 DELETE (service $ dbcBucket rq) rq
    response = receiveEmpty DeleteBucketCorsResponse

data DeleteBucketCorsResponse = DeleteBucketCorsResponse
   deriving (Eq, Show)
