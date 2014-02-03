{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity where

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
getCloudFrontOriginAccessIdentity :: Text
                                  -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity p1 = GetCloudFrontOriginAccessIdentity
    { gcfoairId = p1
    }

data GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity
    { gcfoairId :: !Text
      -- ^ The identity's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetCloudFrontOriginAccessIdentity

instance ToPath GetCloudFrontOriginAccessIdentity where
    toPath GetCloudFrontOriginAccessIdentity{..} = Text.concat
        [ "/2013-11-11/origin-access-identity/cloudfront/"
        , toText gcfoairId
        ]

instance ToQuery GetCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToXML GetCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions

instance AWSRequest GetCloudFrontOriginAccessIdentity where
    type Er GetCloudFrontOriginAccessIdentity = CloudFrontError
    type Rs GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentityResponse
    request = getRestXML service

data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { gcfoairrsCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , gcfoairrsETag :: Maybe Text
      -- ^ The current version of the origin access identity's information. For
      -- example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance FromXML GetCloudFrontOriginAccessIdentityResponse where
    fromXMLOptions = xmlOptions
