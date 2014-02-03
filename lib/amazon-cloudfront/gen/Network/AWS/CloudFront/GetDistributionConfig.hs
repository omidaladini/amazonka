{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a distribution.
module Network.AWS.CloudFront.GetDistributionConfig where

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
getDistributionConfig :: Text
                      -> GetDistributionConfig
getDistributionConfig p1 = GetDistributionConfig
    { gdcrId = p1
    }

data GetDistributionConfig = GetDistributionConfig
    { gdcrId :: !Text
      -- ^ The distribution's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetDistributionConfig

instance ToPath GetDistributionConfig where
    toPath GetDistributionConfig{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText gdcrId
        , "/config"
        ]

instance ToQuery GetDistributionConfig where
    toQuery = const mempty

instance ToXML GetDistributionConfig where
    toXMLOptions = xmlOptions

instance AWSRequest GetDistributionConfig where
    type Er GetDistributionConfig = CloudFrontError
    type Rs GetDistributionConfig = GetDistributionConfigResponse
    request = getRestXML service

data GetDistributionConfigResponse = GetDistributionConfigResponse
    { gdcrrsDistributionConfig :: Maybe DistributionConfig
      -- ^ The distribution's configuration information.
    , gdcrrsETag :: Maybe Text
      -- ^ The current version of the configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance FromXML GetDistributionConfigResponse where
    fromXMLOptions = xmlOptions
