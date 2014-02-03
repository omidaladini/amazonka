{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new distribution.
module Network.AWS.CloudFront.CreateDistribution where

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
createDistribution :: DistributionConfig
                   -> CreateDistribution
createDistribution p1 = CreateDistribution
    { cdrDistributionConfig = p1
    }

data CreateDistribution = CreateDistribution
    { cdrDistributionConfig :: DistributionConfig
      -- ^ The distribution's configuration information.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateDistribution

instance ToPath CreateDistribution where
    toPath = const "/2013-11-11/distribution"

instance ToQuery CreateDistribution where
    toQuery = const mempty

instance ToXML CreateDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest CreateDistribution where
    type Er CreateDistribution = CloudFrontError
    type Rs CreateDistribution = CreateDistributionResponse
    request = postRestXML service

data CreateDistributionResponse = CreateDistributionResponse
    { cdrrsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , cdrrsETag :: Maybe Text
      -- ^ The current version of the distribution created.
    , cdrrsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new distribution resource just created. For
      -- example:
      -- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDistributionResponse where
    fromXMLOptions = xmlOptions
