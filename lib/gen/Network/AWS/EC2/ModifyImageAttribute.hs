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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyImageAttribute :: Text
                     -- ^ The ID of the AMI whose attribute you want to modify.
                     -> ModifyImageAttribute
modifyImageAttribute p1 = ModifyImageAttribute
    { miaImageId = p1
    , miaAttribute = Nothing
    , miaDescription = Nothing
    , miaDryRun = Nothing
    , miaLaunchPermission = Nothing
    , miaOperationType = Nothing
    , miaProductCodes = []
    , miaUserGroups = []
    , miaUserIds = []
    , miaValue = Nothing
    }

data ModifyImageAttribute = ModifyImageAttribute
    { miaAttribute :: Maybe Text
      -- ^ The name of the AMI attribute you want to modify. Available attributes:
      -- launchPermission, productCodes.
    , miaDescription :: Maybe AttributeValue
      -- ^ String value.
    , miaDryRun :: Maybe Bool
    , miaImageId :: !Text
      -- ^ The ID of the AMI whose attribute you want to modify.
    , miaLaunchPermission :: Maybe LaunchPermissionModifications
    , miaOperationType :: Maybe Text
      -- ^ The type of operation being requested. Available operation types: add,
      -- remove.
    , miaProductCodes :: [Text]
      -- ^ The list of product codes being added to or removed from the specified AMI.
      -- Only valid when the productCodes attribute is being modified.
    , miaUserGroups :: [Text]
      -- ^ The user group being added to or removed from the list of user groups with
      -- launch permissions for this AMI. Only valid when the launchPermission
      -- attribute is being modified. Available user groups: all.
    , miaUserIds :: [Text]
      -- ^ The AWS user ID being added to or removed from the list of users with
      -- launch permissions for this AMI. Only valid when the launchPermission
      -- attribute is being modified.
    , miaValue :: Maybe Text
      -- ^ The value of the attribute being modified. Only valid when the description
      -- attribute is being modified.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyImageAttribute

instance AWSRequest ModifyImageAttribute where
    type Er ModifyImageAttribute = EC2Error
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse
    request  = postQuery service "ModifyImageAttribute"
    response = responseXML

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyImageAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyImageAttributeResponse"
