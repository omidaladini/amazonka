{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketTagging where

import Control.Lens
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data GetBucketTagging = GetBucketTagging
    { _gbtrBucket :: BucketName
    } deriving (Generic)

makeLenses ''GetBucketTagging

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toBS _gbtrBucket
        ]

instance ToQuery GetBucketTagging

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

data GetBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtoTagSet :: [Tag]
    } deriving (Generic)

makeLenses ''GetBucketTaggingResponse

instance FromXML GetBucketTaggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingResponse

    request = get
    response _ = xmlResponse