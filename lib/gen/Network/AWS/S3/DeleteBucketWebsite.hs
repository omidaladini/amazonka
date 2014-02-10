{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the website configuration from the bucket.
module Network.AWS.S3.DeleteBucketWebsite where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data DeleteBucketWebsite = DeleteBucketWebsite
    { dbwBucket :: !Text
    } deriving (Generic)

instance ToPath DeleteBucketWebsite where
    toPath DeleteBucketWebsite{..} = Text.concat
        [ "/"
        , toText dbwBucket
        ]

instance ToQuery DeleteBucketWebsite where
    toQuery DeleteBucketWebsite{..} = queryFromList
        [ "website"
        ]

instance ToHeaders DeleteBucketWebsite

instance AWSRequest DeleteBucketWebsite where
    type Er DeleteBucketWebsite = S3Error
    type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse
    request rq = s3 DELETE (service $ dbwBucket rq) rq
    response = receiveEmpty DeleteBucketWebsiteResponse

data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse
   deriving (Eq, Show)
