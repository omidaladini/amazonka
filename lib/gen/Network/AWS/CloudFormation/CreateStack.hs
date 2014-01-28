{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the DescribeStacks API. Currently, the limit for stacks is 20
-- stacks per account per region.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CreateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &NotificationARNs.member.1=arn:aws:sns:us-east-1:1234567890:my-topic
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- FIXME: Operation documentation for CreateStack
module Network.AWS.CloudFormation.CreateStack where

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

data CreateStack = CreateStack
    { csiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If your
      -- template contains IAM resources, you must specify the CAPABILITY_IAM value
      -- for this parameter; otherwise, this action returns an
      -- InsufficientCapabilities error. IAM resources are the following:
      -- AWS::IAM::AccessKey, AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User, and
      -- AWS::IAM::UserToGroupAddition.
    , csiDisableRollback :: Maybe Bool
      -- ^ Set to true to disable rollback of the stack if stack creation failed. You
      -- can specify either DisableRollback or OnFailure, but not both. Default:
      -- false.
    , csiNotificationARNs :: [Text]
      -- ^ The Simple Notification Service (SNS) topic ARNs to publish stack related
      -- events. You can find your SNS topic ARNs using the SNS console or your
      -- Command Line Interface (CLI).
    , csiOnFailure :: Maybe OnFailure
      -- ^ Determines what action will be taken if stack creation fails. This must be
      -- one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either OnFailure
      -- or DisableRollback, but not both. Default: ROLLBACK.
    , csiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for the stack.
    , csiStackName :: !Text
      -- ^ The name associated with the stack. The name must be unique within your AWS
      -- account. Must contain only alphanumeric characters (case sensitive) and
      -- start with an alpha character. Maximum length of the name is 255
      -- characters.
    , csiStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. (For more information, go to
      -- the AWS CloudFormation User Guide.) If you pass StackPolicyBody and
      -- StackPolicyURL, only StackPolicyBody is used.
    , csiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the stack policy. The URL must point to a
      -- policy (max size: 16KB) located in an S3 bucket in the same region as the
      -- stack. If you pass StackPolicyBody and StackPolicyURL, only StackPolicyBody
      -- is used.
    , csiTags :: [Tag]
      -- ^ A set of user-defined Tags to associate with this stack, represented by
      -- key/value pairs. Tags defined for the stack are propagated to EC2 resources
      -- that are created as part of the stack. A maximum number of 10 tags can be
      -- specified.
    , csiTemplateBody :: Maybe Text
      -- ^ Structure containing the template body. (For more information, go to the
      -- AWS CloudFormation User Guide.) Conditional: You must pass TemplateBody or
      -- TemplateURL. If both are passed, only TemplateBody is used.
    , csiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point to a
      -- template (max size: 307,200 bytes) located in an S3 bucket in the same
      -- region as the stack. For more information, go to the AWS CloudFormation
      -- User Guide. Conditional: You must pass TemplateURL or TemplateBody. If both
      -- are passed, only TemplateBody is used.
    , csiTimeoutInMinutes :: Maybe Int
      -- ^ The amount of time that can pass before the stack status becomes
      -- CREATE_FAILED; if DisableRollback is not set or is set to false, the stack
      -- will be rolled back.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateStack

instance AWSRequest CreateStack where
    type Er CreateStack = CloudFormationError
    type Rs CreateStack = CreateStackResponse
    request = getQuery service "CreateStack"

data CreateStackResponse = CreateStackResponse
    { csirsStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Eq, Show, Generic)

instance FromXML CreateStackResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateStackResponse"
        :| ["CreateStackResult"]
