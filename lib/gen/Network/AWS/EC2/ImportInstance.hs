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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
importInstance :: PlatformValues
               -> ImportInstance
importInstance p1 = ImportInstance
    { iirPlatform = p1
    , iirDescription = Nothing
    , iirDiskImages = []
    , iirDryRun = Nothing
    , iirLaunchSpecification = Nothing
    }

data ImportInstance = ImportInstance
    { iirDescription :: Maybe Text
    , iirDiskImages :: [DiskImage]
    , iirDryRun :: Maybe Bool
    , iirLaunchSpecification :: Maybe ImportInstanceLaunchSpecification
    , iirPlatform :: !PlatformValues
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstance

instance AWSRequest ImportInstance where
    type Er ImportInstance = EC2Error
    type Rs ImportInstance = ImportInstanceResponse
    request = getQuery service "ImportInstance"

data ImportInstanceResponse = ImportInstanceResponse
    { iirrsConversionTask :: Maybe ConversionTask
    } deriving (Eq, Show, Generic)

instance FromXML ImportInstanceResponse where
    fromXMLOptions = xmlOptions
