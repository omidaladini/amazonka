{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets a stack policy for a specified stack.
module Network.AWS.CloudFormation.SetStackPolicy where

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

import Network.AWS.CloudFormation.Service
import Network.AWS.CloudFormation.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
setStackPolicy :: Text
               -> SetStackPolicy
setStackPolicy p1 = undefined $ SetStackPolicy
    { sspiStackName = p1
    , sspiStackPolicyBody = Nothing
    , sspiStackPolicyURL = Nothing
    }

data SetStackPolicy = SetStackPolicy
    { sspiStackName :: !Text
      -- ^ The name or stack ID that you want to associate a policy with.
    , sspiStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. (For more information, go to
      -- the AWS CloudFormation User Guide.) You must pass StackPolicyBody or
      -- StackPolicyURL. If both are passed, only StackPolicyBody is used.
    , sspiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the stack policy. The URL must point to a
      -- policy (max size: 16KB) located in an S3 bucket in the same region as the
      -- stack. You must pass StackPolicyBody or StackPolicyURL. If both are passed,
      -- only StackPolicyBody is used.
    } deriving (Eq, Show, Generic)

instance ToQuery SetStackPolicy

instance AWSRequest SetStackPolicy where
    type Er SetStackPolicy = CloudFormationError
    type Rs SetStackPolicy = SetStackPolicyResponse
    request = getQuery service "SetStackPolicy"

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML SetStackPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetStackPolicyResponse"
        :| ["SetStackPolicyResult"]
