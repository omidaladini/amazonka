{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
module Network.AWS.S3.PutBucketLifecycle where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketLifecycle :: Text
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { pbldBucket = p1
    , pbldContentMD5 = Nothing
    , pbldLifecycleConfiguration = Nothing
    }

data PutBucketLifecycle = PutBucketLifecycle
    { pbldBucket :: !Text
    , pbldContentMD5 :: Maybe Text
    , pbldLifecycleConfiguration :: Maybe LifecycleConfiguration
    } deriving (Generic)

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = Text.concat
        [ "/"
        , toText pbldBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery PutBucketLifecycle{..} = queryFromList
        [ "lifecycle"
        ]

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} =
        [ "Content-MD5" =: pbldContentMD5
        ]

instance AWSRequest PutBucketLifecycle where
    type Er PutBucketLifecycle = S3Error
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse
    request rq = s3XML PUT (service $ pbldBucket rq) (pbldLifecycleConfiguration rq) rq
    response = receiveEmpty PutBucketLifecycleResponse

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
   deriving (Eq, Show)
