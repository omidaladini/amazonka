{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.ListBuckets where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

type GetService = ListBuckets
type GetServiceResponse = ListBucketsResponse

data ListBuckets = ListBuckets
    deriving (Generic)

instance ToPath ListBuckets where
    toPath = const "/"

instance ToQuery ListBuckets where
    toQuery = const mempty

instance ToHeaders ListBuckets

instance AWSRequest ListBuckets where
    type Er ListBuckets = S3Error
    type Rs ListBuckets = ListBucketsResponse
    request = s3 GET (service "")
    response = receiveXML $ \hs doc -> ListBucketsResponse
        <$> xml "Buckets" doc
        <*> xml "Owner" doc

data ListBucketsResponse = ListBucketsResponse
    { lboBuckets :: [Bucket]
    , lboOwner :: Maybe Owner
    } deriving (Eq, Show)
