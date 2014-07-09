{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketLifecycle where

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


-- | Default GetBucketLifecycle request.
getBucketLifecycle :: BucketName -- ^ 'gblrBucket'
                   -> GetBucketLifecycle
getBucketLifecycle p1 = GetBucketLifecycle
    { gblrBucket = p1
    }

data GetBucketLifecycle = GetBucketLifecycle
    { gblrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = mconcat
        [ "/"
        , toBS gblrBucket
        ]

instance ToQuery GetBucketLifecycle

instance ToHeaders GetBucketLifecycle

instance ToBody GetBucketLifecycle

instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3

    request  = get
    response = bodyResponse $ \hs bdy ->
        return $! pure GetBucketLifecycleResponse
            <*> pure bdy

data instance Rs GetBucketLifecycle = GetBucketLifecycleResponse
    { gbloRules :: [Rule]
    } deriving (Eq, Show, Generic)