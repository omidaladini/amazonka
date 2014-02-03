{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete an origin access identity.
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity where

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
deleteCloudFrontOriginAccessIdentity :: Text
                                     -> Text
                                     -> DeleteCloudFrontOriginAccessIdentity
deleteCloudFrontOriginAccessIdentity p1 p2 = DeleteCloudFrontOriginAccessIdentity
    { dcfoairId = p1
    , dcfoairIfMatch = p2
    }

data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity
    { dcfoairId :: !Text
      -- ^ The origin access identity's id.
    , dcfoairIfMatch :: !Text
      -- ^ The value of the ETag header you received from a previous GET or PUT
      -- request. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteCloudFrontOriginAccessIdentity where
    toHeaders DeleteCloudFrontOriginAccessIdentity{..} =
        [ "If-Match" =: dcfoairIfMatch
        ]

instance ToPath DeleteCloudFrontOriginAccessIdentity where
    toPath DeleteCloudFrontOriginAccessIdentity{..} = Text.concat
        [ "/2013-11-11/origin-access-identity/cloudfront/"
        , toText dcfoairId
        ]

instance ToQuery DeleteCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToXML DeleteCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions

instance AWSRequest DeleteCloudFrontOriginAccessIdentity where
    type Er DeleteCloudFrontOriginAccessIdentity = CloudFrontError
    type Rs DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentityResponse
    request = deleteRestXML service

data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteCloudFrontOriginAccessIdentityResponse where
    fromXMLOptions = xmlOptions
