{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the website configuration for a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketWebsite where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data GetBucketWebsite = GetBucketWebsite
    { _gbwrBucket :: BucketName
    } deriving (Show, Generic)

makeLenses ''GetBucketWebsite

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toBS _gbwrBucket
        ]

instance ToQuery GetBucketWebsite where
    toQuery GetBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders GetBucketWebsite

instance ToBody GetBucketWebsite

data GetBucketWebsiteResponse = GetBucketWebsiteResponse
    { _gbwoErrorDocument :: Maybe ErrorDocument
    , _gbwoIndexDocument :: Maybe IndexDocument
    , _gbwoRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _gbwoRoutingRules :: [RoutingRule]
    } deriving (Show, Generic)

makeLenses ''GetBucketWebsiteResponse

instance FromXML GetBucketWebsiteResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3
    type Rs GetBucketWebsite = GetBucketWebsiteResponse

    request = get
    response _ = xmlResponse