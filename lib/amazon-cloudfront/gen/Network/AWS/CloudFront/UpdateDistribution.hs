{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a distribution.
module Network.AWS.CloudFront.UpdateDistribution where

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
updateDistribution :: DistributionConfig
                   -> Text
                   -> Text
                   -> UpdateDistribution
updateDistribution p1 p2 p3 = UpdateDistribution
    { udrDistributionConfig = p1
    , udrId = p2
    , udrIfMatch = p3
    }

data UpdateDistribution = UpdateDistribution
    { udrDistributionConfig :: DistributionConfig
      -- ^ The distribution's configuration information.
    , udrId :: !Text
      -- ^ The distribution's id.
    , udrIfMatch :: !Text
      -- ^ The value of the ETag header you received when retrieving the
      -- distribution's configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance ToHeaders UpdateDistribution where
    toHeaders UpdateDistribution{..} =
        [ "If-Match" =: udrIfMatch
        ]

instance ToPath UpdateDistribution where
    toPath UpdateDistribution{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText udrId
        , "/config"
        ]

instance ToQuery UpdateDistribution where
    toQuery = const mempty

instance ToXML UpdateDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest UpdateDistribution where
    type Er UpdateDistribution = CloudFrontError
    type Rs UpdateDistribution = UpdateDistributionResponse
    request = putRestXML service

data UpdateDistributionResponse = UpdateDistributionResponse
    { udrrsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , udrrsETag :: Maybe Text
      -- ^ The current version of the configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateDistributionResponse where
    fromXMLOptions = xmlOptions
