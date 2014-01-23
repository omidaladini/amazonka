{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets an attribute of an instance to its default value.
module Network.AWS.EC2.ResetInstanceAttribute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ResetInstanceAttribute = ResetInstanceAttribute
    { riarAttribute :: !InstanceAttributeName
      -- ^ The name of the attribute being reset. Available attribute names: kernel,
      -- ramdisk.
    , riarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , riarInstanceId :: !Text
      -- ^ The ID of the Amazon EC2 instance whose attribute is being reset.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetInstanceAttribute

instance AWSRequest ResetInstanceAttribute where
    type Er ResetInstanceAttribute = EC2Error
    type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse
    request = v2Query service GET "ResetInstanceAttribute"

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ResetInstanceAttributeResponse where
    fromXMLOptions = xmlOptions
