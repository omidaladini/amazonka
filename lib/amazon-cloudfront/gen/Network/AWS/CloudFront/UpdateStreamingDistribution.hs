{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a streaming distribution.
module Network.AWS.CloudFront.UpdateStreamingDistribution where

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
updateStreamingDistribution :: Text
                            -> Text
                            -> StreamingDistributionConfig
                            -> UpdateStreamingDistribution
updateStreamingDistribution p1 p2 p3 = UpdateStreamingDistribution
    { usdrId = p1
    , usdrIfMatch = p2
    , usdrStreamingDistributionConfig = p3
    }

data UpdateStreamingDistribution = UpdateStreamingDistribution
    { usdrId :: !Text
      -- ^ The streaming distribution's id.
    , usdrIfMatch :: !Text
      -- ^ The value of the ETag header you received when retrieving the streaming
      -- distribution's configuration. For example: E2QWRUHAPOMQZL.
    , usdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Eq, Show, Generic)

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} =
        [ "If-Match" =: usdrIfMatch
        ]

instance ToPath UpdateStreamingDistribution where
    toPath UpdateStreamingDistribution{..} = Text.concat
        [ "/2013-11-11/streaming-distribution/"
        , toText usdrId
        , "/config"
        ]

instance ToQuery UpdateStreamingDistribution where
    toQuery = const mempty

instance ToXML UpdateStreamingDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest UpdateStreamingDistribution where
    type Er UpdateStreamingDistribution = CloudFrontError
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResponse
    request = putRestXML service

data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { usdrrsETag :: Maybe Text
      -- ^ The current version of the configuration. For example: E2QWRUHAPOMQZL.
    , usdrrsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateStreamingDistributionResponse where
    fromXMLOptions = xmlOptions
