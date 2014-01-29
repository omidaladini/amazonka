{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribePermissions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the permissions for a specified stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribePermissions where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describePermissions :: DescribePermissions
describePermissions = DescribePermissions
    { dprIamUserArn = Nothing
    , dprStackId = Nothing
    }

data DescribePermissions = DescribePermissions
    { dprIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN. For more information about IAM ARNs, see Using
      -- Identifiers.
    , dprStackId :: Maybe Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribePermissions

instance AWSRequest DescribePermissions where
    type Er DescribePermissions = OpsWorksError
    type Rs DescribePermissions = DescribePermissionsResponse
    request  = getJSON service
    response = responseJSON

data DescribePermissionsResponse = DescribePermissionsResponse
    { dprrsPermissions :: [Permission]
      -- ^ An array of Permission objects that describe the stack permissions. If the
      -- request object contains only a stack ID, the array contains a Permission
      -- object with permissions for each of the stack IAM ARNs. If the request
      -- object contains only an IAM ARN, the array contains a Permission object
      -- with permissions for each of the user's stack IDs. If the request contains
      -- a stack ID and an IAM ARN, the array contains a single Permission object
      -- with permissions for the specified stack and IAM ARN.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribePermissionsResponse
