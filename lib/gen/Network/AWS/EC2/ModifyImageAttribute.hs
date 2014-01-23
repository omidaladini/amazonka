{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyImageAttribute operation modifies an attribute of an AMI.
module Network.AWS.EC2.ModifyImageAttribute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ModifyImageAttribute = ModifyImageAttribute
    { miarAttribute :: Maybe Text
      -- ^ The name of the AMI attribute you want to modify. Available attributes:
      -- launchPermission, productCodes.
    , miarDescription :: Maybe AttributeValue
      -- ^ String value.
    , miarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , miarImageId :: !Text
      -- ^ The ID of the AMI whose attribute you want to modify.
    , miarLaunchPermission :: Maybe LaunchPermissionModifications
      -- ^ FIXME: Missing documentation
    , miarOperationType :: Maybe Text
      -- ^ The type of operation being requested. Available operation types: add,
      -- remove.
    , miarProductCodes :: [Text]
      -- ^ The list of product codes being added to or removed from the specified AMI.
      -- Only valid when the productCodes attribute is being modified.
    , miarUserGroups :: [Text]
      -- ^ The user group being added to or removed from the list of user groups with
      -- launch permissions for this AMI. Only valid when the launchPermission
      -- attribute is being modified. Available user groups: all.
    , miarUserIds :: [Text]
      -- ^ The AWS user ID being added to or removed from the list of users with
      -- launch permissions for this AMI. Only valid when the launchPermission
      -- attribute is being modified.
    , miarValue :: Maybe Text
      -- ^ The value of the attribute being modified. Only valid when the description
      -- attribute is being modified.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyImageAttribute

instance AWSRequest ModifyImageAttribute where
    type Er ModifyImageAttribute = EC2Error
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse
    request = v2Query service GET "ModifyImageAttribute"

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyImageAttributeResponse where
    fromXMLOptions = xmlOptions
