{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
-- https://iam.amazonaws.com/ ?Action=GetAccountPasswordPolicy
-- &Version=2010-05-08 &AUTHPARAMS 6, false false false false true
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetAccountPasswordPolicy where

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
getAccountPasswordPolicy :: AWS (Either IAMError GetAccountPasswordPolicyResponse)
getAccountPasswordPolicy = undefined GetAccountPasswordPolicy

data GetAccountPasswordPolicy = GetAccountPasswordPolicy
    deriving (Eq, Show, Generic)

instance ToQuery GetAccountPasswordPolicy

instance AWSRequest GetAccountPasswordPolicy where
    type Er GetAccountPasswordPolicy = IAMError
    type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse
    request = getQuery service "GetAccountPasswordPolicy"

data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse
    { gapprPasswordPolicy :: PasswordPolicy
      -- ^ The PasswordPolicy data type contains information about the account
      -- password policy. This data type is used as a response element in the action
      -- GetAccountPasswordPolicy.
    } deriving (Eq, Show, Generic)

instance FromXML GetAccountPasswordPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetAccountPasswordPolicyResponse"
        :| ["GetAccountPasswordPolicyResult"]
