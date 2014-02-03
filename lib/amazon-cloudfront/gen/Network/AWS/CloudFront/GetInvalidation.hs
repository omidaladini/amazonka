{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an invalidation.
module Network.AWS.CloudFront.GetInvalidation where

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
getInvalidation :: Text
                -> Text
                -> GetInvalidation
getInvalidation p1 p2 = GetInvalidation
    { girDistributionId = p1
    , girId = p2
    }

data GetInvalidation = GetInvalidation
    { girDistributionId :: !Text
      -- ^ The distribution's id.
    , girId :: !Text
      -- ^ The invalidation's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetInvalidation

instance ToPath GetInvalidation where
    toPath GetInvalidation{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText girDistributionId
        , "/invalidation/"
        , toText girId
        ]

instance ToQuery GetInvalidation where
    toQuery = const mempty

instance ToXML GetInvalidation where
    toXMLOptions = xmlOptions

instance AWSRequest GetInvalidation where
    type Er GetInvalidation = CloudFrontError
    type Rs GetInvalidation = GetInvalidationResponse
    request = getRestXML service

data GetInvalidationResponse = GetInvalidationResponse
    { girrsInvalidation :: Maybe Invalidation
      -- ^ The invalidation's information.
    } deriving (Eq, Show, Generic)

instance FromXML GetInvalidationResponse where
    fromXMLOptions = xmlOptions
