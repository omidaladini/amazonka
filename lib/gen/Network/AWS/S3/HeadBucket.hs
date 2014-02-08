{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.HeadBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation is useful to determine if a bucket exists and you have
-- permission to access it.
module Network.AWS.S3.HeadBucket where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
headBucket :: Text
           -> HeadBucket
headBucket p1 = HeadBucket
    { hbBucket = p1
    }

data HeadBucket = HeadBucket
    { hbBucket :: !Text
    } deriving (Generic)

instance ToHeaders HeadBucket

instance ToPath HeadBucket where
    toPath HeadBucket{..} = Text.concat
        [ "/"
        , toText hbBucket
        ]

instance ToQuery HeadBucket where
    toQuery = const mempty

instance AWSRequest HeadBucket where
    type Er HeadBucket = S3Error
    type Rs HeadBucket = HeadBucketResponse
    request  = headS3 service
    response = undefined

data HeadBucketResponse = HeadBucketResponse
    deriving (Eq, Show, Generic)

instance FromXML HeadBucketResponse where
    fromXMLOptions = xmlOptions
