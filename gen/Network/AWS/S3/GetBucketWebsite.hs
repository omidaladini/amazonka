{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the website configuration for a bucket.
module Network.AWS.S3.GetBucketWebsite where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetBucketWebsite = GetBucketWebsite
    { gbwBucket :: !Text
    } deriving (Generic)

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = Text.concat
        [ "/"
        , toText gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery GetBucketWebsite{..} = queryFromList
        [ "website"
        ]

instance ToHeaders GetBucketWebsite

instance AWSRequest GetBucketWebsite where
    type Er GetBucketWebsite = S3Error
    type Rs GetBucketWebsite = GetBucketWebsiteResponse
    request rq = s3 GET (service $ gbwBucket rq) rq
    response = receiveXML $ \hs doc -> GetBucketWebsiteResponse
        <$> xml "ErrorDocument" doc
        <*> xml "IndexDocument" doc
        <*> xml "RedirectAllRequestsTo" doc
        <*> xml "RoutingRules" doc

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { gbwrErrorDocument :: Maybe ErrorDocument
    , gbwrIndexDocument :: Maybe IndexDocument
    , gbwrRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , gbwrRoutingRules :: [RoutingRule]
    } deriving (Eq, Show)
