{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new origin access identity.
module Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoairCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoaisCloudFrontOriginAccessIdentity
    , ccfoaisLocation
    , ccfoaisETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCloudFrontOriginAccessIdentity' request.
createCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoairCloudFrontOriginAccessIdentityConfig'
                                     -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity p1 = CreateCloudFrontOriginAccessIdentity
    { _ccfoairCloudFrontOriginAccessIdentityConfig = p1
    }

data CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The origin access identity's configuration information.
    } deriving (Show, Generic)

-- | The origin access identity's configuration information.
ccfoairCloudFrontOriginAccessIdentityConfig
    :: Functor f
    => (CloudFrontOriginAccessIdentityConfig
    -> f (CloudFrontOriginAccessIdentityConfig))
    -> CreateCloudFrontOriginAccessIdentity
    -> f CreateCloudFrontOriginAccessIdentity
ccfoairCloudFrontOriginAccessIdentityConfig f x =
    (\y -> x { _ccfoairCloudFrontOriginAccessIdentityConfig = y })
       <$> f (_ccfoairCloudFrontOriginAccessIdentityConfig x)
{-# INLINE ccfoairCloudFrontOriginAccessIdentityConfig #-}

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToXML CreateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateCloudFrontOriginAccessIdentityRequest"

data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoaisCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , _ccfoaisLocation :: Maybe Text
      -- ^ The fully qualified URI of the new origin access identity just
      -- created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
      -- 
    , _ccfoaisETag :: Maybe Text
      -- ^ The current version of the origin access identity created.
    } deriving (Show, Generic)

-- | The origin access identity's information.
ccfoaisCloudFrontOriginAccessIdentity
    :: Functor f
    => (Maybe CloudFrontOriginAccessIdentity
    -> f (Maybe CloudFrontOriginAccessIdentity))
    -> CreateCloudFrontOriginAccessIdentityResponse
    -> f CreateCloudFrontOriginAccessIdentityResponse
ccfoaisCloudFrontOriginAccessIdentity f x =
    (\y -> x { _ccfoaisCloudFrontOriginAccessIdentity = y })
       <$> f (_ccfoaisCloudFrontOriginAccessIdentity x)
{-# INLINE ccfoaisCloudFrontOriginAccessIdentity #-}

-- | The fully qualified URI of the new origin access identity just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoaisLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCloudFrontOriginAccessIdentityResponse
    -> f CreateCloudFrontOriginAccessIdentityResponse
ccfoaisLocation f x =
    (\y -> x { _ccfoaisLocation = y })
       <$> f (_ccfoaisLocation x)
{-# INLINE ccfoaisLocation #-}

-- | The current version of the origin access identity created.
ccfoaisETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateCloudFrontOriginAccessIdentityResponse
    -> f CreateCloudFrontOriginAccessIdentityResponse
ccfoaisETag f x =
    (\y -> x { _ccfoaisETag = y })
       <$> f (_ccfoaisETag x)
{-# INLINE ccfoaisETag #-}

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
