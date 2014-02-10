{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteSecurityGroup operation deletes a security group. If you attempt
-- to delete a security group that contains instances, a fault is returned. If
-- you attempt to delete a security group that is referenced by another
-- security group, a fault is returned. For example, if security group B has a
-- rule that allows access from security group A, security group A cannot be
-- deleted until the allow rule is removed.
module Network.AWS.EC2.DeleteSecurityGroup where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeleteSecurityGroup = DeleteSecurityGroup
    { dsgDryRun :: Maybe Bool
    , dsgGroupId :: Maybe Text
      -- ^ The ID of the Amazon EC2 security group to delete.
    , dsgGroupName :: Maybe Text
      -- ^ The name of the Amazon EC2 security group to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSecurityGroup

instance AWSRequest DeleteSecurityGroup where
    type Er DeleteSecurityGroup = EC2Error
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
    request  = postQuery service "DeleteSecurityGroup"
    response = responseXML

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSecurityGroupResponse"