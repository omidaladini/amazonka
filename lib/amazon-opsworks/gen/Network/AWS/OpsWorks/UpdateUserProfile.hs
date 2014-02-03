{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified user profile. Required Permissions: To use this action,
-- an IAM user must have an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.UpdateUserProfile where

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
updateUserProfile :: Text
                  -> UpdateUserProfile
updateUserProfile p1 = UpdateUserProfile
    { uuprIamUserArn = p1
    , uuprAllowSelfManagement = Nothing
    , uuprSshPublicKey = Nothing
    , uuprSshUsername = Nothing
    }

data UpdateUserProfile = UpdateUserProfile
    { uuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My Settings
      -- page. For more information, see Managing User Permissions.
    , uuprIamUserArn :: !Text
      -- ^ The user IAM ARN.
    , uuprSshPublicKey :: Maybe Text
      -- ^ The user's new SSH public key.
    , uuprSshUsername :: Maybe Text
      -- ^ The user's new SSH user name.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateUserProfile where
    toJSON = genericToJSON jsonOptions

instance AWSRequest UpdateUserProfile where
    type Er UpdateUserProfile = OpsWorksError
    type Rs UpdateUserProfile = UpdateUserProfileResponse
    request  = getJSON service
    response = responseJSON

data UpdateUserProfileResponse = UpdateUserProfileResponse
    deriving (Eq, Show, Generic)

instance FromJSON UpdateUserProfileResponse where
    fromJSON = genericFromJSON jsonOptions

