{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the tags for a bucket.
module Network.AWS.S3.PutBucketTagging where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketTagging :: Text
                 -> Tagging
                 -> PutBucketTagging
putBucketTagging p1 p2 = PutBucketTagging
    { pbtBucket = p1
    , pbtTagging = p2
    , pbtContentMD5 = Nothing
    }

data PutBucketTagging = PutBucketTagging
    { pbtBucket :: !Text
    , pbtContentMD5 :: Maybe Text
    , pbtTagging :: Tagging
    } deriving (Generic)

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = Text.concat
        [ "/"
        , toText pbtBucket
        ]

instance ToQuery PutBucketTagging where
    toQuery PutBucketTagging{..} = queryFromList
        [ "tagging"
        ]

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} =
        [ "Content-MD5" =: pbtContentMD5
        ]

instance AWSRequest PutBucketTagging where
    type Er PutBucketTagging = S3Error
    type Rs PutBucketTagging = PutBucketTaggingResponse
    request rq = s3XML PUT (service $ pbtBucket rq) (pbtTagging rq) rq
    response = receiveEmpty PutBucketTaggingResponse

data PutBucketTaggingResponse = PutBucketTaggingResponse
   deriving (Eq, Show)