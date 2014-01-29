{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the policy that grants an entity permission to assume a role.
-- Currently, only an Amazon EC2 instance can assume a role. For more
-- information about roles, go to Working with Roles.
-- https://iam.amazonaws.com/ ?Action=UpdateAssumeRolePolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 309c1671-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.UpdateAssumeRolePolicy where

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
updateAssumeRolePolicy :: Text
                       -> Text
                       -> UpdateAssumeRolePolicy
updateAssumeRolePolicy p1 p2 = undefined $ UpdateAssumeRolePolicy
    { uarprPolicyDocument = p1
    , uarprRoleName = p2
    }

data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { uarprPolicyDocument :: !Text
      -- ^ The policy that grants an entity permission to assume the role.
    , uarprRoleName :: !Text
      -- ^ Name of the role to update.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateAssumeRolePolicy

instance AWSRequest UpdateAssumeRolePolicy where
    type Er UpdateAssumeRolePolicy = IAMError
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse
    request = getQuery service "UpdateAssumeRolePolicy"

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateAssumeRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateAssumeRolePolicyResponse"
        :| ["UpdateAssumeRolePolicyResult"]
