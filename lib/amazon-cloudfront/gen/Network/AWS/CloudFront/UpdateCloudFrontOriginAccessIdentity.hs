{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update an origin access identity.
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity where

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
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig
                                     -> Text
                                     -> Text
                                     -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity p1 p2 p3 = UpdateCloudFrontOriginAccessIdentity
    { ucfoairCloudFrontOriginAccessIdentityConfig = p1
    , ucfoairId = p2
    , ucfoairIfMatch = p3
    }

data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { ucfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The identity's configuration information.
    , ucfoairId :: !Text
      -- ^ The identity's id.
    , ucfoairIfMatch :: !Text
      -- ^ The value of the ETag header you received when retrieving the identity's
      -- configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance ToHeaders UpdateCloudFrontOriginAccessIdentity where
    toHeaders UpdateCloudFrontOriginAccessIdentity{..} =
        [ "If-Match" =: ucfoairIfMatch
        ]

instance ToPath UpdateCloudFrontOriginAccessIdentity where
    toPath UpdateCloudFrontOriginAccessIdentity{..} = Text.concat
        [ "/2013-11-11/origin-access-identity/cloudfront/"
        , toText ucfoairId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToXML UpdateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Er UpdateCloudFrontOriginAccessIdentity = CloudFrontError
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse
    request = putRestXML service

data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { ucfoairrsCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , ucfoairrsETag :: Maybe Text
      -- ^ The current version of the configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateCloudFrontOriginAccessIdentityResponse where
    fromXMLOptions = xmlOptions
