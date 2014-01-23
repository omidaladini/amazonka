{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ActivateLicense
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Activates a specific number of licenses for a 90-day period. Activations
-- can be done against a specific license ID.
module Network.AWS.EC2.ActivateLicense where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ActivateLicense = ActivateLicense
    { alrCapacity :: !Int
      -- ^ Specifies the additional number of licenses to activate.
    , alrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , alrLicenseId :: !Text
      -- ^ Specifies the ID for the specific license to activate against.
    } deriving (Eq, Show, Generic)

instance ToQuery ActivateLicense

instance AWSRequest ActivateLicense where
    type Er ActivateLicense = EC2Error
    type Rs ActivateLicense = ActivateLicenseResponse
    request = v2Query service GET "ActivateLicense"

data ActivateLicenseResponse = ActivateLicenseResponse
    deriving (Eq, Show, Generic)

instance FromXML ActivateLicenseResponse where
    fromXMLOptions = xmlOptions
