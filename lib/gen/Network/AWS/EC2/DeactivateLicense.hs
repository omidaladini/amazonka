{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeactivateLicense
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deactivates a specific number of licenses. Deactivations can be done
-- against a specific license ID after they have persisted for at least a
-- 90-day period.
module Network.AWS.EC2.DeactivateLicense where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeactivateLicense = DeactivateLicense
    { dlsCapacity :: !Int
      -- ^ Specifies the amount of capacity to deactivate against the license.
    , dlsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dlsLicenseId :: !Text
      -- ^ Specifies the ID for the specific license to deactivate against.
    } deriving (Eq, Show, Generic)

instance ToQuery DeactivateLicense

instance AWSRequest DeactivateLicense where
    type Er DeactivateLicense = EC2Error
    type Rs DeactivateLicense = DeactivateLicenseResponse
    request = v2Query service GET "DeactivateLicense"

data DeactivateLicenseResponse = DeactivateLicenseResponse
    deriving (Eq, Show, Generic)

instance FromXML DeactivateLicenseResponse where
    fromXMLOptions = xmlOptions
