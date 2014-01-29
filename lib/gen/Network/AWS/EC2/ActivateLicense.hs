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

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
activateLicense :: Int
                -> Text
                -> ActivateLicense
activateLicense p1 p2 = undefined $ ActivateLicense
    { alrCapacity = p1
    , alrLicenseId = p2
    , alrDryRun = Nothing
    }

data ActivateLicense = ActivateLicense
    { alrCapacity :: !Int
      -- ^ Specifies the additional number of licenses to activate.
    , alrDryRun :: Maybe Bool
    , alrLicenseId :: !Text
      -- ^ Specifies the ID for the specific license to activate against.
    } deriving (Eq, Show, Generic)

instance ToQuery ActivateLicense

instance AWSRequest ActivateLicense where
    type Er ActivateLicense = EC2Error
    type Rs ActivateLicense = ActivateLicenseResponse
    request = getQuery service "ActivateLicense"

data ActivateLicenseResponse = ActivateLicenseResponse
    deriving (Eq, Show, Generic)

instance FromXML ActivateLicenseResponse where
    fromXMLOptions = xmlOptions
