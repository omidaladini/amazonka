{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the policy of a specified bucket.
module Network.AWS.S3.GetBucketPolicy where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketPolicy = GetBucketPolicy
    { gbpBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = Text.concat
        [ "/"
        , toText gbpBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery GetBucketPolicy{..} = queryFromList
        [ "policy"
        ]

instance ToHeaders GetBucketPolicy

instance AWSRequest GetBucketPolicy where
    type Er GetBucketPolicy = S3Error
    type Rs GetBucketPolicy = GetBucketPolicyResponse
    request rq = s3 GET (service $ gbpBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketPolicyResponse
        <$> xml "Policy" doc

data GetBucketPolicyResponse = GetBucketPolicyResponse
    { gbprPolicy :: Maybe Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Eq, Show)
