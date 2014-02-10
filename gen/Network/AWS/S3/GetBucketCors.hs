{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the cors configuration for the bucket.
module Network.AWS.S3.GetBucketCors where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketCors = GetBucketCors
    { gbcBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = Text.concat
        [ "/"
        , toText gbcBucket
        ]

instance ToQuery GetBucketCors where
    toQuery GetBucketCors{..} = queryFromList
        [ "cors"
        ]

instance ToHeaders GetBucketCors

instance AWSRequest GetBucketCors where
    type Er GetBucketCors = S3Error
    type Rs GetBucketCors = GetBucketCorsResponse
    request rq = s3 GET (service $ gbcBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketCorsResponse
        <$> xml "CORSRules" doc

data GetBucketCorsResponse = GetBucketCorsResponse
    { gbcrCORSRule :: [CORSRule]
    } deriving (Eq, Show)
