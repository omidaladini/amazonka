{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.BundleInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The BundleInstance operation request that an instance is bundled the next
-- time it boots. The bundling process creates a new image from a running
-- instance and stores the AMI data in S3. Once bundled, the image must be
-- registered in the normal way using the RegisterImage API.
module Network.AWS.EC2.BundleInstance where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
bundleInstance :: Text
               -- ^ The ID of the instance to bundle.
               -> Storage
               -> BundleInstance
bundleInstance p1 p2 = BundleInstance
    { biInstanceId = p1
    , biStorage = p2
    , biDryRun = Nothing
    }

data BundleInstance = BundleInstance
    { biDryRun :: Maybe Bool
    , biInstanceId :: !Text
      -- ^ The ID of the instance to bundle.
    , biStorage :: Storage
    } deriving (Eq, Show, Generic)

instance ToQuery BundleInstance

instance AWSRequest BundleInstance where
    type Er BundleInstance = EC2Error
    type Rs BundleInstance = BundleInstanceResponse
    request = getQuery service "BundleInstance"

data BundleInstanceResponse = BundleInstanceResponse
    { birBundleTask :: Maybe BundleTask
    } deriving (Eq, Show, Generic)

instance FromXML BundleInstanceResponse where
    fromXMLOptions = xmlOptions
