{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketPolicy
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
module Network.AWS.S3.V2006_03_01.PutBucketPolicy where

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


-- | Default PutBucketPolicy request.
putBucketPolicy :: Text -- ^ 'pbprPolicy'
                -> BucketName -- ^ 'pbprBucket'
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { pbprPolicy = p1
    , pbprBucket = p2
    , pbprContentMD5 = Nothing
    }

data PutBucketPolicy = PutBucketPolicy
    { pbprPolicy :: Text
      -- ^ The bucket policy as a JSON document.
    , pbprBucket :: BucketName
    , pbprContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toBS pbprBucket
        ]

instance ToQuery PutBucketPolicy

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = concat
        [ "Content-MD5" =: pbprContentMD5
        ]

instance ToBody PutBucketPolicy where
    toBody = undefined -- toBody . pbprPolicy

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3

    request  = put
    response = headerResposne $ const PutBucketPolicyResponse

data instance Rs PutBucketPolicy = PutBucketPolicyResponse
    deriving (Eq, Show, Generic)