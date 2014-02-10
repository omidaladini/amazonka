{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
module Network.AWS.S3.PutBucketWebsite where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketWebsite :: Text
                 -> WebsiteConfiguration
                 -> PutBucketWebsite
putBucketWebsite p1 p2 = PutBucketWebsite
    { pbwBucket = p1
    , pbwWebsiteConfiguration = p2
    , pbwContentMD5 = Nothing
    }

data PutBucketWebsite = PutBucketWebsite
    { pbwBucket :: !Text
    , pbwContentMD5 :: Maybe Text
    , pbwWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Generic)

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = Text.concat
        [ "/"
        , toText pbwBucket
        ]

instance ToQuery PutBucketWebsite where
    toQuery PutBucketWebsite{..} = queryFromList
        [ "website"
        ]

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} =
        [ "Content-MD5" =: pbwContentMD5
        ]

instance AWSRequest PutBucketWebsite where
    type Er PutBucketWebsite = S3Error
    type Rs PutBucketWebsite = PutBucketWebsiteResponse
    request rq = s3XML PUT (service $ pbwBucket rq) (pbwWebsiteConfiguration rq) rq
    response = receiveEmpty PutBucketWebsiteResponse

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
   deriving (Eq, Show)
