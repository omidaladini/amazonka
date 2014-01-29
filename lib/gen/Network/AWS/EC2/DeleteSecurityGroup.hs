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
deleteSecurityGroup :: AWS (Either EC2Error DeleteSecurityGroupResponse)
deleteSecurityGroup = undefined $ DeleteSecurityGroup
    { dsgrDryRun = Nothing
    , dsgrGroupId = Nothing
    , dsgrGroupName = Nothing
    }

data DeleteSecurityGroup = DeleteSecurityGroup
    { dsgrDryRun :: Maybe Bool
    , dsgrGroupId :: Maybe Text
      -- ^ The ID of the Amazon EC2 security group to delete.
    , dsgrGroupName :: Maybe Text
      -- ^ The name of the Amazon EC2 security group to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSecurityGroup

instance AWSRequest DeleteSecurityGroup where
    type Er DeleteSecurityGroup = EC2Error
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse
    request = getQuery service "DeleteSecurityGroup"

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSecurityGroupResponse where
    fromXMLOptions = xmlOptions
