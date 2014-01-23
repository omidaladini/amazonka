{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ImportInstance
module Network.AWS.EC2.ImportInstance where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ImportInstance = ImportInstance
    { iirDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iirDiskImages :: [DiskImage]
      -- ^ FIXME: Missing documentation
    , iirDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , iirLaunchSpecification :: Maybe ImportInstanceLaunchSpecification
      -- ^ FIXME: Missing documentation
    , iirPlatform :: !PlatformValues
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstance

instance AWSRequest ImportInstance where
    type Er ImportInstance = EC2Error
    type Rs ImportInstance = ImportInstanceResponse
    request = v2Query service GET "ImportInstance"

data ImportInstanceResponse = ImportInstanceResponse
    { iirrsConversionTask :: Maybe ConversionTask
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML ImportInstanceResponse where
    fromXMLOptions = xmlOptions
