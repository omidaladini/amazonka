{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates an alias for your AWS account. For information about
-- using an AWS account alias, see Using an Alias for Your AWS Account ID in
-- Using AWS Identity and Access Management. https://iam.amazonaws.com/
-- ?Action=CreateAccountAlias &AccountAlias=foocorporation &Version=2010-05-08
-- &AUTHPARAMS 36b5db08-f1b0-11df-8fbe-45274EXAMPLE.
module Network.AWS.IAM.CreateAccountAlias where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createAccountAlias :: Text
                   -- ^ Name of the account alias to create.
                   -> CreateAccountAlias
createAccountAlias p1 = CreateAccountAlias
    { caarAccountAlias = p1
    }

data CreateAccountAlias = CreateAccountAlias
    { caarAccountAlias :: !Text
      -- ^ Name of the account alias to create.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateAccountAlias

instance AWSRequest CreateAccountAlias where
    type Er CreateAccountAlias = IAMError
    type Rs CreateAccountAlias = CreateAccountAliasResponse
    request = getQuery service "CreateAccountAlias"

data CreateAccountAliasResponse = CreateAccountAliasResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateAccountAliasResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateAccountAliasResponse"
