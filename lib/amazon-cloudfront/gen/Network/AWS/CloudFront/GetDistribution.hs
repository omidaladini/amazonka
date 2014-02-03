{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a distribution.
module Network.AWS.CloudFront.GetDistribution where

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
getDistribution :: Text
                -> GetDistribution
getDistribution p1 = GetDistribution
    { gdrId = p1
    }

data GetDistribution = GetDistribution
    { gdrId :: !Text
      -- ^ The distribution's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetDistribution

instance ToPath GetDistribution where
    toPath GetDistribution{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText gdrId
        ]

instance ToQuery GetDistribution where
    toQuery = const mempty

instance ToXML GetDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest GetDistribution where
    type Er GetDistribution = CloudFrontError
    type Rs GetDistribution = GetDistributionResponse
    request = getRestXML service

data GetDistributionResponse = GetDistributionResponse
    { gdrrsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , gdrrsETag :: Maybe Text
      -- ^ The current version of the distribution's information. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance FromXML GetDistributionResponse where
    fromXMLOptions = xmlOptions
