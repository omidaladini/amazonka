{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the password policy settings for the account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
-- https://iam.amazonaws.com/ ?Action=UpdateAccountPasswordPolicy
-- &MinimumPasswordLength=9 &RequireSymbols=true &RequireNumbers=false
-- &RequireUppercaseCharacters=true &RequireLowercaseCharacters=true
-- &AllowUsersToChangePassword=true &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateAccountPasswordPolicy where

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

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { uapprAllowUsersToChangePassword :: Maybe Bool
    , uapprMinimumPasswordLength :: Maybe Int
    , uapprRequireLowercaseCharacters :: Maybe Bool
    , uapprRequireNumbers :: Maybe Bool
    , uapprRequireSymbols :: Maybe Bool
    , uapprRequireUppercaseCharacters :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateAccountPasswordPolicy

instance AWSRequest UpdateAccountPasswordPolicy where
    type Er UpdateAccountPasswordPolicy = IAMError
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse
    request = getQuery service "UpdateAccountPasswordPolicy"

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateAccountPasswordPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateAccountPasswordPolicyResponse"
        :| ["UpdateAccountPasswordPolicyResult"]
