{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRolePolicy &PolicyName=S3AccessPolicy &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS S3AccessPolicy S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:*"],"Resource":["*"]}]}
-- 7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.GetRolePolicy where

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

-- | Convenience method utilising default fields where applicable.
getRolePolicy :: Text
              -> Text
              -> AWS (Either IAMError GetRolePolicyResponse)
getRolePolicy p1 p2 = undefined $ GetRolePolicy
    { grprPolicyName = p1
    , grprRoleName = p2
    }

data GetRolePolicy = GetRolePolicy
    { grprPolicyName :: !Text
      -- ^ Name of the policy document to get.
    , grprRoleName :: !Text
      -- ^ Name of the role associated with the policy.
    } deriving (Eq, Show, Generic)

instance ToQuery GetRolePolicy

instance AWSRequest GetRolePolicy where
    type Er GetRolePolicy = IAMError
    type Rs GetRolePolicy = GetRolePolicyResponse
    request = getQuery service "GetRolePolicy"

data GetRolePolicyResponse = GetRolePolicyResponse
    { grprrsPolicyDocument :: !Text
      -- ^ The policy document.
    , grprrsPolicyName :: !Text
      -- ^ The name of the policy.
    , grprrsRoleName :: !Text
      -- ^ The role the policy is associated with.
    } deriving (Eq, Show, Generic)

instance FromXML GetRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetRolePolicyResponse"
        :| ["GetRolePolicyResult"]
