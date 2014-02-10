{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
module Network.AWS.S3.DeleteBucketPolicy where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data DeleteBucketPolicy = DeleteBucketPolicy
    { dbpBucket :: !Text
    } deriving (Generic)

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = Text.concat
        [ "/"
        , toText dbpBucket
        ]

instance ToQuery DeleteBucketPolicy where
    toQuery DeleteBucketPolicy{..} = queryFromList
        [ "policy"
        ]

instance ToHeaders DeleteBucketPolicy

instance AWSRequest DeleteBucketPolicy where
    type Er DeleteBucketPolicy = S3Error
    type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
    request rq = s3 DELETE (service $ dbpBucket rq) rq
    response = receiveEmpty DeleteBucketPolicyResponse

data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse
   deriving (Eq, Show)
