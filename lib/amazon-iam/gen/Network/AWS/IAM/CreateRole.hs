{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new role for your AWS account. For more information about roles,
-- go to Working with Roles. For information about limitations on role names
-- and the number of roles you can create, go to Limitations on IAM Entities
-- in Using AWS Identity and Access Management. The policy grants permission
-- to an EC2 instance to assume the role. The policy is URL-encoded according
-- to RFC 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. Currently, only EC2 instances can
-- assume roles. https://iam.amazonaws.com/ ?Action=CreateRole
-- &RoleName=S3Access &Path=/application_abc/component_xyz/
-- &AssumeRolePolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &Version=2010-05-08 &AUTHPARAMS /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01.495Z AROADBQP57FF2AEXAMPLE
-- 4a93ceee-9966-11e1-b624-b1aEXAMPLE7c.
module Network.AWS.IAM.CreateRole where

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
createRole :: Text
           -> Text
           -> CreateRole
createRole p1 p2 = CreateRole
    { crrAssumeRolePolicyDocument = p1
    , crrRoleName = p2
    , crrPath = Nothing
    }

data CreateRole = CreateRole
    { crrAssumeRolePolicyDocument :: !Text
      -- ^ The policy that grants an entity permission to assume the role.
    , crrPath :: Maybe Text
      -- ^ The path to the role. For more information about paths, see Identifiers for
      -- IAM Entities in Using AWS Identity and Access Management. This parameter is
      -- optional. If it is not included, it defaults to a slash (/).
    , crrRoleName :: !Text
      -- ^ Name of the role to create.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateRole

instance AWSRequest CreateRole where
    type Er CreateRole = IAMError
    type Rs CreateRole = CreateRoleResponse
    request = getQuery service "CreateRole"

data CreateRoleResponse = CreateRoleResponse
    { crrrsRole :: Role
      -- ^ Information about the role.
    } deriving (Eq, Show, Generic)

instance FromXML CreateRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateRoleResponse"
        :| ["CreateRoleResult"]
