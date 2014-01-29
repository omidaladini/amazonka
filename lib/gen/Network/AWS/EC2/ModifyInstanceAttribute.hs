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

-- | Convenience method utilising default fields where applicable.
modifyInstanceAttribute :: Text
                        -> AWS (Either EC2Error ModifyInstanceAttributeResponse)
modifyInstanceAttribute p1 = undefined $ ModifyInstanceAttribute
    { miasInstanceId = p1
    , miasAttribute = Nothing
    , miasBlockDeviceMappings = []
    , miasDisableApiTermination = Nothing
    , miasDryRun = Nothing
    , miasEbsOptimized = Nothing
    , miasGroups = []
    , miasInstanceInitiatedShutdownBehavior = Nothing
    , miasInstanceType = Nothing
    , miasKernel = Nothing
    , miasRamdisk = Nothing
    , miasSourceDestCheck = Nothing
    , miasSriovNetSupport = Nothing
    , miasUserData = Nothing
    , miasValue = Nothing
    }

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { miasAttribute :: Maybe InstanceAttributeName
      -- ^ The name of the attribute being modified. Available attribute names:
      -- instanceType, kernel, ramdisk, userData, disableApiTermination,
      -- instanceInitiatedShutdownBehavior, rootDevice, blockDeviceMapping.
    , miasBlockDeviceMappings :: [InstanceBlockDeviceMappingSpecification]
      -- ^ The new block device mappings for the instance whose attributes are being
      -- modified. Only valid when blockDeviceMapping is specified as the attribute
      -- being modified.
    , miasDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miasDryRun :: Maybe Bool
    , miasEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miasGroups :: [Text]
    , miasInstanceId :: !Text
      -- ^ The ID of the instance whose attribute is being modified.
    , miasInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ String value.
    , miasInstanceType :: Maybe AttributeValue
      -- ^ String value.
    , miasKernel :: Maybe AttributeValue
      -- ^ String value.
    , miasRamdisk :: Maybe AttributeValue
      -- ^ String value.
    , miasSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , miasSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    , miasUserData :: Maybe AttributeValue
      -- ^ String value.
    , miasValue :: Maybe Text
      -- ^ The new value of the instance attribute being modified. Only valid when
      -- kernel, ramdisk, userData, disableApiTermination or
      -- instanceInitiateShutdownBehavior is specified as the attribute being
      -- modified.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyInstanceAttribute

instance AWSRequest ModifyInstanceAttribute where
    type Er ModifyInstanceAttribute = EC2Error
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse
    request = getQuery service "ModifyInstanceAttribute"

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyInstanceAttributeResponse where
    fromXMLOptions = xmlOptions
