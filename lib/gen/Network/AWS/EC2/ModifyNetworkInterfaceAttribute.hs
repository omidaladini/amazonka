{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ModifyNetworkInterfaceAttribute
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { mniarAttachment :: Maybe NetworkInterfaceAttachmentChanges
      -- ^ FIXME: Missing documentation
    , mniarDescription :: Maybe AttributeValue
      -- ^ String value.
    , mniarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , mniarGroups :: [Text]
      -- ^ FIXME: Missing documentation
    , mniarNetworkInterfaceId :: !Text
      -- ^ FIXME: Missing documentation
    , mniarSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyNetworkInterfaceAttribute

instance AWSRequest ModifyNetworkInterfaceAttribute where
    type Er ModifyNetworkInterfaceAttribute = EC2Error
    type Rs ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttributeResponse
    request = v2Query service GET "ModifyNetworkInterfaceAttribute"

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions
