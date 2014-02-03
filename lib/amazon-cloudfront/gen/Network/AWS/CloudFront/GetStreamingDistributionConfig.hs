{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a streaming distribution.
module Network.AWS.CloudFront.GetStreamingDistributionConfig where

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
getStreamingDistributionConfig :: Text
                               -> GetStreamingDistributionConfig
getStreamingDistributionConfig p1 = GetStreamingDistributionConfig
    { gsdcrId = p1
    }

data GetStreamingDistributionConfig = GetStreamingDistributionConfig
    { gsdcrId :: !Text
      -- ^ The streaming distribution's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetStreamingDistributionConfig

instance ToPath GetStreamingDistributionConfig where
    toPath GetStreamingDistributionConfig{..} = Text.concat
        [ "/2013-11-11/streaming-distribution/"
        , toText gsdcrId
        , "/config"
        ]

instance ToQuery GetStreamingDistributionConfig where
    toQuery = const mempty

instance ToXML GetStreamingDistributionConfig where
    toXMLOptions = xmlOptions

instance AWSRequest GetStreamingDistributionConfig where
    type Er GetStreamingDistributionConfig = CloudFrontError
    type Rs GetStreamingDistributionConfig = GetStreamingDistributionConfigResponse
    request = getRestXML service

data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { gsdcrrsETag :: Maybe Text
      -- ^ The current version of the configuration. For example: E2QWRUHAPOMQZL.
    , gsdcrrsStreamingDistributionConfig :: Maybe StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Eq, Show, Generic)

instance FromXML GetStreamingDistributionConfigResponse where
    fromXMLOptions = xmlOptions
