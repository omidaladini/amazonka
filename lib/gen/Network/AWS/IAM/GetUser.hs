{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified user, including the user's path,
-- unique ID, and ARN. If you do not specify a user name, IAM determines the
-- user name implicitly based on the AWS access key ID signing the request.
-- https://iam.amazonaws.com/ ?Action=GetUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetUser where

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
getUser :: GetUser
getUser = GetUser
    { gurUserName = Nothing
    }

data GetUser = GetUser
    { gurUserName :: Maybe Text
      -- ^ Name of the user to get information about. This parameter is optional. If
      -- it is not included, it defaults to the user making the request.
    } deriving (Eq, Show, Generic)

instance ToQuery GetUser

instance AWSRequest GetUser where
    type Er GetUser = IAMError
    type Rs GetUser = GetUserResponse
    request = getQuery service "GetUser"

data GetUserResponse = GetUserResponse
    { gurrsUser :: User
      -- ^ Information about the user.
    } deriving (Eq, Show, Generic)

instance FromXML GetUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetUserResponse"
        :| ["GetUserResult"]
