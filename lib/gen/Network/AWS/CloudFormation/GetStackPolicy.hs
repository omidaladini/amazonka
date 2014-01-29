{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the stack policy for a specified stack. If a stack doesn't have a
-- policy, a null value is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetStackPolicy
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "Statement" : [ { "Effect" : "Deny", "Action" :
-- "Update*", "Resource" : "LogicalResourceId/ProductionDatabase" }, {
-- "Effect" : "Allow", "Action" : "Update:*", "Resource" : "*" } ] }.
module Network.AWS.CloudFormation.GetStackPolicy where

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
getStackPolicy :: Text
               -> GetStackPolicy
getStackPolicy p1 = undefined $ GetStackPolicy
    { gspiStackName = p1
    }

data GetStackPolicy = GetStackPolicy
    { gspiStackName :: !Text
      -- ^ The name or stack ID that is associated with the stack whose policy you
      -- want to get.
    } deriving (Eq, Show, Generic)

instance ToQuery GetStackPolicy

instance AWSRequest GetStackPolicy where
    type Er GetStackPolicy = CloudFormationError
    type Rs GetStackPolicy = GetStackPolicyResponse
    request = getQuery service "GetStackPolicy"

data GetStackPolicyResponse = GetStackPolicyResponse
    { gspirsStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. (For more information, go to
      -- the AWS CloudFormation User Guide.).
    } deriving (Eq, Show, Generic)

instance FromXML GetStackPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetStackPolicyResponse"
        :| ["GetStackPolicyResult"]
