{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new user profile. Required Permissions: To use this action, an
-- IAM user must have an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.CreateUserProfile where

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
createUserProfile :: Text
                  -> AWS (Either OpsWorksError CreateUserProfileResponse)
createUserProfile p1 = undefined $ CreateUserProfile
    { cuprIamUserArn = p1
    , cuprAllowSelfManagement = Nothing
    , cuprSshPublicKey = Nothing
    , cuprSshUsername = Nothing
    }

data CreateUserProfile = CreateUserProfile
    { cuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My Settings
      -- page. For more information, see .
    , cuprIamUserArn :: !Text
      -- ^ The user's IAM ARN.
    , cuprSshPublicKey :: Maybe Text
      -- ^ The user's public SSH key.
    , cuprSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateUserProfile

instance AWSRequest CreateUserProfile where
    type Er CreateUserProfile = OpsWorksError
    type Rs CreateUserProfile = CreateUserProfileResponse
    request  = getJSON service
    response = responseJSON

data CreateUserProfileResponse = CreateUserProfileResponse
    { cuprrsIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateUserProfileResponse
