{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack via the DescribeStacks action. Note: You cannot update
-- AWS::S3::Bucket resources, for example, to add or modify tags. To get a
-- copy of the template for an existing stack, you can use the GetTemplate
-- action. Tags that were associated with this stack during creation time will
-- still be associated with the stack after an UpdateStack operation. For more
-- information about creating an update template, updating a stack, and
-- monitoring the progress of the update, see Updating a Stack.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=UpdateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- FIXME: Operation documentation for UpdateStack
module Network.AWS.CloudFormation.UpdateStack where

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

data UpdateStack = UpdateStack
    { usiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If your stack
      -- contains IAM resources, you must specify the CAPABILITY_IAM value for this
      -- parameter; otherwise, this action returns an InsufficientCapabilities
      -- error. IAM resources are the following: AWS::IAM::AccessKey,
      -- AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User, and
      -- AWS::IAM::UserToGroupAddition.
    , usiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for the stack.
    , usiStackName :: !Text
      -- ^ The name or stack ID of the stack to update. Must contain only alphanumeric
      -- characters (case sensitive) and start with an alpha character. Maximum
      -- length of the name is 255 characters.
    , usiStackPolicyBody :: Maybe Text
      -- ^ Structure containing the updated stack policy body. If you pass
      -- StackPolicyBody and StackPolicyURL, only StackPolicyBody is used. If you
      -- want to update a stack policy during a stack update, specify an updated
      -- stack policy. For example, you can include an updated stack policy to
      -- protect a new resource created in the stack update. If you do not specify a
      -- stack policy, the current policy that is associated with the stack is
      -- unchanged.
    , usiStackPolicyDuringUpdateBody :: Maybe Text
      -- ^ Structure containing the temporary overriding stack policy body. If you
      -- pass StackPolicyDuringUpdateBody and StackPolicyDuringUpdateURL, only
      -- StackPolicyDuringUpdateBody is used. If you want to update protected
      -- resources, specify a temporary overriding stack policy during this update.
      -- If you do not specify a stack policy, the current policy that associated
      -- with the stack will be used.
    , usiStackPolicyDuringUpdateURL :: Maybe Text
      -- ^ Location of a file containing the temporary overriding stack policy. The
      -- URL must point to a policy (max size: 16KB) located in an S3 bucket in the
      -- same region as the stack. If you pass StackPolicyDuringUpdateBody and
      -- StackPolicyDuringUpdateURL, only StackPolicyDuringUpdateBody is used. If
      -- you want to update protected resources, specify a temporary overriding
      -- stack policy during this update. If you do not specify a stack policy, the
      -- current policy that is associated with the stack will be used.
    , usiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the updated stack policy. The URL must point
      -- to a policy (max size: 16KB) located in an S3 bucket in the same region as
      -- the stack. If you pass StackPolicyBody and StackPolicyURL, only
      -- StackPolicyBody is used. If you want to update a stack policy during a
      -- stack update, specify an updated stack policy. For example, you can include
      -- an updated stack policy to protect a new resource created in the stack
      -- update. If you do not specify a stack policy, the current policy that is
      -- associated with the stack is unchanged.
    , usiTemplateBody :: Maybe Text
      -- ^ Structure containing the template body. (For more information, go to the
      -- AWS CloudFormation User Guide.) Conditional: You must pass TemplateBody or
      -- TemplateURL. If both are passed, only TemplateBody is used.
    , usiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point to a
      -- template located in an S3 bucket in the same region as the stack. For more
      -- information, go to the AWS CloudFormation User Guide. Conditional: You must
      -- pass TemplateURL or TemplateBody. If both are passed, only TemplateBody is
      -- used.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateStack

instance AWSRequest UpdateStack where
    type Er UpdateStack = CloudFormationError
    type Rs UpdateStack = UpdateStackResponse
    request = getQuery service "UpdateStack"

data UpdateStackResponse = UpdateStackResponse
    { usirsStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateStackResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateStackResponse"
        :| ["UpdateStackResult"]
