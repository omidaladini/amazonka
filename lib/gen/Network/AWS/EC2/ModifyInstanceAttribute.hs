{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an attribute of an instance.
module Network.AWS.EC2.ModifyInstanceAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyInstanceAttribute :: Text
                        -- ^ The ID of the instance whose attribute is being modified.
                        -> ModifyInstanceAttribute
modifyInstanceAttribute p1 = ModifyInstanceAttribute
    { miadInstanceId = p1
    , miadAttribute = Nothing
    , miadBlockDeviceMappings = []
    , miadDisableApiTermination = Nothing
    , miadDryRun = Nothing
    , miadEbsOptimized = Nothing
    , miadGroups = []
    , miadInstanceInitiatedShutdownBehavior = Nothing
    , miadInstanceType = Nothing
    , miadKernel = Nothing
    , miadRamdisk = Nothing
    , miadSourceDestCheck = Nothing
    , miadSriovNetSupport = Nothing
    , miadUserData = Nothing
    , miadValue = Nothing
    }

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { miadAttribute :: Maybe InstanceAttributeName
      -- ^ The name of the attribute being modified. Available attribute names:
      -- instanceType, kernel, ramdisk, userData, disableApiTermination,
      -- instanceInitiatedShutdownBehavior, rootDevice, blockDeviceMapping.
    , miadBlockDeviceMappings :: [InstanceBlockDeviceMappingSpecification]
      -- ^ The new block device mappings for the instance whose attributes are being
      -- modified. Only valid when blockDeviceMapping is specified as the attribute
      -- being modified.
    , miadDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miadDryRun :: Maybe Bool
    , miadEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miadGroups :: [Text]
    , miadInstanceId :: !Text
      -- ^ The ID of the instance whose attribute is being modified.
    , miadInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ String value.
    , miadInstanceType :: Maybe AttributeValue
      -- ^ String value.
    , miadKernel :: Maybe AttributeValue
      -- ^ String value.
    , miadRamdisk :: Maybe AttributeValue
      -- ^ String value.
    , miadSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miadSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    , miadUserData :: Maybe AttributeValue
      -- ^ String value.
    , miadValue :: Maybe Text
      -- ^ The new value of the instance attribute being modified. Only valid when
      -- kernel, ramdisk, userData, disableApiTermination or
      -- instanceInitiateShutdownBehavior is specified as the attribute being
      -- modified.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyInstanceAttribute

instance AWSRequest ModifyInstanceAttribute where
    type Er ModifyInstanceAttribute = EC2Error
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse
    request  = postQuery service "ModifyInstanceAttribute"
    response = responseXML

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyInstanceAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyInstanceAttributeResponse"
