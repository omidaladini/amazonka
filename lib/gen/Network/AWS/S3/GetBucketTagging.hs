{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.GetBucketTagging where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketTagging = GetBucketTagging
    { gbtBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = Text.concat
        [ "/"
        , toText gbtBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery GetBucketTagging{..} = queryFromList
        [ "tagging"
        ]

instance ToHeaders GetBucketTagging

instance AWSRequest GetBucketTagging where
    type Er GetBucketTagging = S3Error
    type Rs GetBucketTagging = GetBucketTaggingResponse
    request rq = s3 GET (service $ gbtBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketTaggingResponse
        <$> xml "TagSet" doc

data GetBucketTaggingResponse = GetBucketTaggingResponse
    { gbtrTagSet :: [Tag]
    } deriving (Eq, Show)