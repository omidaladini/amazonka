{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new origin access identity.
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.CloudFront.Service
import Network.AWS.CloudFront.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig
                                     -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity p1 = CreateCloudFrontOriginAccessIdentity
    { ccfoairCloudFrontOriginAccessIdentityConfig = p1
    }

data CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { ccfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The origin access identity's configuration information.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2013-11-11/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToXML CreateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Er CreateCloudFrontOriginAccessIdentity = CloudFrontError
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse
    request = postRestXML service

data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { ccfoairrsCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , ccfoairrsETag :: Maybe Text
      -- ^ The current version of the origin access identity created.
    , ccfoairrsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new origin access identity just created. For
      -- example:
      -- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCloudFrontOriginAccessIdentityResponse where
    fromXMLOptions = xmlOptions
