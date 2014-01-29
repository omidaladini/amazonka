{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified user. You should
-- understand the implications of changing a user's path or name. For more
-- information, see Renaming Users and Groups in Using AWS Identity and Access
-- Management. To change a user name the requester must have appropriate
-- permissions on both the source object and the target object. For example,
-- to change Bob to Robert, the entity making the request must have permission
-- on Bob and Robert, or must have permission on all (*). For more information
-- about permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateUser &UserName=Bob &NewUserName=Robert &Version=2010-05-08
-- &AUTHPARAMS /division_abc/subdivision_xyz/ Robert AIDACKCEVSQ6C2EXAMPLE
-- arn:aws::123456789012:user/division_abc/subdivision_xyz/Robert
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateUser where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateUser :: Text
           -> UpdateUser
updateUser p1 = undefined $ UpdateUser
    { uurUserName = p1
    , uurNewPath = Nothing
    , uurNewUserName = Nothing
    }

data UpdateUser = UpdateUser
    { uurNewPath :: Maybe Text
      -- ^ New path for the user. Include this parameter only if you're changing the
      -- user's path.
    , uurNewUserName :: Maybe Text
      -- ^ New name for the user. Include this parameter only if you're changing the
      -- user's name.
    , uurUserName :: !Text
      -- ^ Name of the user to update. If you're changing the name of the user, this
      -- is the original user name.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateUser

instance AWSRequest UpdateUser where
    type Er UpdateUser = IAMError
    type Rs UpdateUser = UpdateUserResponse
    request = getQuery service "UpdateUser"

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateUserResponse"
        :| ["UpdateUserResult"]
