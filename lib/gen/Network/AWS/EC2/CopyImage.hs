{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CopyImage
module Network.AWS.EC2.CopyImage where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
copyImage :: Text
          -> Text
          -> CopyImage
copyImage p1 p2 = CopyImage
    { cirSourceImageId = p1
    , cirSourceRegion = p2
    , cirClientToken = Nothing
    , cirDescription = Nothing
    , cirDryRun = Nothing
    , cirName = Nothing
    }

data CopyImage = CopyImage
    { cirClientToken :: Maybe Text
    , cirDescription :: Maybe Text
    , cirDryRun :: Maybe Bool
    , cirName :: Maybe Text
    , cirSourceImageId :: !Text
    , cirSourceRegion :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CopyImage

instance AWSRequest CopyImage where
    type Er CopyImage = EC2Error
    type Rs CopyImage = CopyImageResponse
    request = getQuery service "CopyImage"

data CopyImageResponse = CopyImageResponse
    { cirrImageId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML CopyImageResponse where
    fromXMLOptions = xmlOptions
