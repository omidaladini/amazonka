{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in Using AWS
-- Identity and Access Management. https://iam.amazonaws.com/
-- ?Action=DeleteAccountAlias &AccountAlias=foocorporation &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteAccountAlias where

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

data DeleteAccountAlias = DeleteAccountAlias
    { daarAccountAlias :: !Text
      -- ^ Name of the account alias to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteAccountAlias

instance AWSRequest DeleteAccountAlias where
    type Er DeleteAccountAlias = IAMError
    type Rs DeleteAccountAlias = DeleteAccountAliasResponse
    request = getQuery service "DeleteAccountAlias"

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAccountAliasResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteAccountAliasResponse"
        :| ["DeleteAccountAliasResult"]
