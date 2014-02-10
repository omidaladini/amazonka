{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.GetBucketLifecycle where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketLifecycle = GetBucketLifecycle
    { gbldBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = Text.concat
        [ "/"
        , toText gbldBucket
        ]

instance ToQuery GetBucketLifecycle where
    toQuery GetBucketLifecycle{..} = queryFromList
        [ "lifecycle"
        ]

instance ToHeaders GetBucketLifecycle

instance AWSRequest GetBucketLifecycle where
    type Er GetBucketLifecycle = S3Error
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse
    request rq = s3 GET (service $ gbldBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketLifecycleResponse
        <$> xml "Rules" doc

data GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { gbldrRule :: [Rule]
    } deriving (Eq, Show)
