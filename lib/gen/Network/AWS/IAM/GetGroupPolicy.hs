{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified group. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS Admins AdminRoot
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetGroupPolicy where

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

data GetGroupPolicy = GetGroupPolicy
    { ggprGroupName :: !Text
      -- ^ Name of the group the policy is associated with.
    , ggprPolicyName :: !Text
      -- ^ Name of the policy document to get.
    } deriving (Eq, Show, Generic)

instance ToQuery GetGroupPolicy

instance AWSRequest GetGroupPolicy where
    type Er GetGroupPolicy = IAMError
    type Rs GetGroupPolicy = GetGroupPolicyResponse
    request = getQuery service "GetGroupPolicy"

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { ggprrsGroupName :: !Text
      -- ^ The group the policy is associated with.
    , ggprrsPolicyDocument :: !Text
      -- ^ The policy document.
    , ggprrsPolicyName :: !Text
      -- ^ The name of the policy.
    } deriving (Eq, Show, Generic)

instance FromXML GetGroupPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetGroupPolicyResponse"
        :| ["GetGroupPolicyResult"]
