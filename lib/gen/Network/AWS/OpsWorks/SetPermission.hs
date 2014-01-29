{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specifies a stack's permissions. For more information, see Security and
-- Permissions. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.SetPermission where

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

-- | Convenience method utilising default fields where applicable.
setPermission :: Text
              -> Text
              -> AWS (Either OpsWorksError SetPermissionResponse)
setPermission p1 p2 = undefined $ SetPermission
    { sprIamUserArn = p1
    , sprStackId = p2
    , sprAllowSsh = Nothing
    , sprAllowSudo = Nothing
    , sprLevel = Nothing
    }

data SetPermission = SetPermission
    { sprAllowSsh :: Maybe Bool
      -- ^ The user is allowed to use SSH to communicate with the instance.
    , sprAllowSudo :: Maybe Bool
      -- ^ The user is allowed to use sudo to elevate privileges.
    , sprIamUserArn :: !Text
      -- ^ The user's IAM ARN.
    , sprLevel :: Maybe Text
      -- ^ The user's permission level, which must be set to one of the following
      -- strings. You cannot set your own permissions level. deny show deploy manage
      -- iam_only For more information on the permissions associated with these
      -- levels, see Managing User Permissions.
    , sprStackId :: !Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance ToJSON SetPermission

instance AWSRequest SetPermission where
    type Er SetPermission = OpsWorksError
    type Rs SetPermission = SetPermissionResponse
    request  = getJSON service
    response = responseJSON

data SetPermissionResponse = SetPermissionResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetPermissionResponse
