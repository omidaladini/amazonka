{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new user for your AWS account. For information about limitations
-- on the number of users you can create, see Limitations on IAM Entities in
-- Using AWS Identity and Access Management. https://iam.amazonaws.com/
-- ?Action=CreateUser &Path=/division_abc/subdivision_xyz/ &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateUser where

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

data CreateUser = CreateUser
    { curPath :: Maybe Text
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
      -- This parameter is optional. If it is not included, it defaults to a slash
      -- (/).
    , curUserName :: !Text
      -- ^ Name of the user to create.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateUser

instance AWSRequest CreateUser where
    type Er CreateUser = IAMError
    type Rs CreateUser = CreateUserResponse
    request = getQuery service "CreateUser"

data CreateUserResponse = CreateUserResponse
    { currsUser :: Maybe User
      -- ^ Information about the user.
    } deriving (Eq, Show, Generic)

instance FromXML CreateUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateUserResponse"
        :| ["CreateUserResult"]