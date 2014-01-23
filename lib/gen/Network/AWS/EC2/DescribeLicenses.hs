{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeLicenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides details of a user's registered licenses. Zero or more IDs may be
-- specified on the call. When one or more license IDs are specified, only
-- data for the specified IDs are returned.
module Network.AWS.EC2.DescribeLicenses where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeLicenses = DescribeLicenses
    { dlrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dlrFilters :: [Filter]
      -- ^ A list of filters used to match properties for Licenses. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dlrLicenseIds :: [Text]
      -- ^ Specifies the license registration for which details are to be returned.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeLicenses

instance AWSRequest DescribeLicenses where
    type Er DescribeLicenses = EC2Error
    type Rs DescribeLicenses = DescribeLicensesResponse
    request = v2Query service GET "DescribeLicenses"

data DescribeLicensesResponse = DescribeLicensesResponse
    { dlrrsLicenses :: [License]
      -- ^ Specifies active licenses in use and attached to an Amazon EC2 instance.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeLicensesResponse where
    fromXMLOptions = xmlOptions
