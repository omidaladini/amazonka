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

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketWebsite :: Text
                 -> GetBucketWebsite
getBucketWebsite p1 = GetBucketWebsite
    { gbwBucket = p1
    }

data GetBucketWebsite = GetBucketWebsite
    { gbwBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketWebsite

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = Text.concat
        [ "/"
        , toText gbwBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery GetBucketWebsite{..} = queryFromList
        [ "website"
        ]

instance AWSRequest GetBucketWebsite where
    type Er GetBucketWebsite = S3Error
    type Rs GetBucketWebsite = GetBucketWebsiteResponse
    request  = getS3 service
    response = undefined

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { gbwrsErrorDocument :: Maybe ErrorDocument
    , gbwrsIndexDocument :: Maybe IndexDocument
    , gbwrsRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , gbwrsRoutingRules :: [RoutingRule]
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions
