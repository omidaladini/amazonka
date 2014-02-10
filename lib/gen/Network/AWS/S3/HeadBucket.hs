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

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data HeadBucket = HeadBucket
    { hbBucket :: !Text
    } deriving (Generic)

instance ToPath HeadBucket where
    toPath HeadBucket{..} = Text.concat
        [ "/"
        , toText hbBucket
        ]

instance ToQuery HeadBucket where
    toQuery = const mempty

instance ToHeaders HeadBucket

instance AWSRequest HeadBucket where
    type Er HeadBucket = S3Error
    type Rs HeadBucket = HeadBucketResponse
    request rq = s3 HEAD (service $ hbBucket rq) rq
    response = receiveEmpty HeadBucketResponse

data HeadBucketResponse = HeadBucketResponse
   deriving (Eq, Show)
