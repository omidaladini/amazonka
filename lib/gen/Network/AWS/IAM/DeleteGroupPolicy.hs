{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy that is associated with the specified group.
-- https://iam.amazonaws.com/ ?Action=DeleteGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteGroupPolicy where

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

data DeleteGroupPolicy = DeleteGroupPolicy
    { dgprGroupName :: !Text
      -- ^ Name of the group the policy is associated with.
    , dgprPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteGroupPolicy

instance AWSRequest DeleteGroupPolicy where
    type Er DeleteGroupPolicy = IAMError
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
    request = getQuery service "DeleteGroupPolicy"

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteGroupPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteGroupPolicyResponse"
        :| ["DeleteGroupPolicyResult"]
