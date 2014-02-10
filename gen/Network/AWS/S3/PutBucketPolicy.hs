{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
module Network.AWS.S3.PutBucketPolicy where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketPolicy :: Text
                -> Text
                -- ^ The bucket policy as a JSON document.
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { pbpBucket = p1
    , pbpPolicy = p2
    , pbpContentMD5 = Nothing
    }

data PutBucketPolicy = PutBucketPolicy
    { pbpBucket :: !Text
    , pbpContentMD5 :: Maybe Text
    , pbpPolicy :: !Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Generic)

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = Text.concat
        [ "/"
        , toText pbpBucket
        ]

instance ToQuery PutBucketPolicy where
    toQuery PutBucketPolicy{..} = queryFromList
        [ "policy"
        ]

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} =
        [ "Content-MD5" =: pbpContentMD5
        ]

instance AWSRequest PutBucketPolicy where
    type Er PutBucketPolicy = S3Error
    type Rs PutBucketPolicy = PutBucketPolicyResponse
    request rq = s3XML PUT (service $ pbpBucket rq) (pbpPolicy rq) rq
    response = receiveEmpty PutBucketPolicyResponse

data PutBucketPolicyResponse = PutBucketPolicyResponse
   deriving (Eq, Show)
