{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) that you can use to
-- access AWS resources that you might not normally have access to. Typically,
-- you use AssumeRole for cross-account access or federation. For
-- cross-account access, imagine that you own multiple accounts and need to
-- access resources in each account. You could create long-term credentials in
-- each account to access those resources. However, managing all those
-- credentials and remembering which one can access which account can be time
-- consuming. Instead, you can create one set of long-term credentials in one
-- account and then use temporary security credentials to access all the other
-- accounts by assuming roles in those accounts. For more information about
-- roles, see Roles in Using IAM. For federation, you can, for example, grant
-- single sign-on access to the AWS Management Console. If you already have an
-- identity and authentication system in your corporate network, you don't
-- have to recreate user identities in AWS in order to grant those user
-- identities access to AWS. Instead, after a user has been authenticated, you
-- call AssumeRole (and specify the role with the appropriate permissions) to
-- get temporary security credentials for that user. With those temporary
-- security credentials, you construct a sign-in URL that users can use to
-- access the console. For more information, see Scenarios for Granting
-- Temporary Access in AWS Security Token Service. The temporary security
-- credentials are valid for the duration that you specified when calling
-- AssumeRole, which can be from 900 seconds (15 minutes) to 3600 seconds (1
-- hour). The default is 1 hour. Optionally, you can pass an AWS IAM access
-- policy to this operation. The temporary security credentials that are
-- returned by the operation have the permissions that are associated with the
-- access policy of the role that is being assumed, except for any permissions
-- explicitly denied by the policy you pass. This gives you a way to further
-- restrict the permissions for the resulting temporary security credentials.
-- These policies and any applicable resource-based policies are evaluated
-- when calls to AWS are made using the temporary security credentials. To
-- assume a role, your AWS account must be trusted by the role. The trust
-- relationship is defined in the role's trust policy when the IAM role is
-- created. You must also have a policy that allows you to call
-- sts:AssumeRole. Important: You cannot call AssumeRole by using AWS account
-- credentials; access will be denied. You must use IAM user credentials or
-- temporary security credentials to call AssumeRole.
-- https://sts.amazonaws.com/ ?Version=2011-06-15 &Action=AssumeRole
-- &RoleSessionName=Bob &RoleArn=arn:aws:iam::123456789012:role/demo
-- &Policy=%7B%22Version%22%3A%222012-10-17%22%2C%22Statement%22%3A%5B%7B%22Sid%22%3A%22Stmt1%22%2C%22Effect%22%
-- 3A%22Allow%22%2C%22Action%22%3A%22s3%3A*%22%2C%22Resource%22%3A%22*%22%7D
-- %5D%7D &DurationSeconds=3600 &ExternalId=123ABC &AUTHPARAMS 2011-06-15/">
-- AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
-- LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
-- QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
-- 9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
-- +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-15T23:28:33.359Z
-- AKIAIOSFODNN7EXAMPLE arn:aws:sts::123456789012:assumed-role/demo/Bob
-- ARO123EXAMPLE123:Bob 6 c6104cbe-af31-11e0-8154-cbc7ccf896c7.
module Network.AWS.STS.AssumeRole where

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

import Network.AWS.STS.Service
import Network.AWS.STS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
assumeRole :: Text
           -> Text
           -> AssumeRole
assumeRole p1 p2 = AssumeRole
    { arrRoleArn = p1
    , arrRoleSessionName = p2
    , arrDurationSeconds = Nothing
    , arrExternalId = Nothing
    , arrPolicy = Nothing
    }

data AssumeRole = AssumeRole
    { arrDurationSeconds :: Maybe Int
      -- ^ The duration, in seconds, of the role session. The value can range from 900
      -- seconds (15 minutes) to 3600 seconds (1 hour). By default, the value is set
      -- to 3600 seconds.
    , arrExternalId :: Maybe Text
      -- ^ A unique identifier that is used by third parties to assume a role in their
      -- customers' accounts. For each role that the third party can assume, they
      -- should instruct their customers to create a role with the external ID that
      -- the third party generated. Each time the third party assumes the role, they
      -- must pass the customer's external ID. The external ID is useful in order to
      -- help third parties bind a role to the customer who created it. For more
      -- information about the external ID, see About the External ID in Using
      -- Temporary Security Credentials.
    , arrPolicy :: Maybe Text
      -- ^ An AWS IAM policy in JSON format. The temporary security credentials that
      -- are returned by the operation have the permissions that are associated with
      -- the access policy of the role being assumed, except for any permissions
      -- explicitly denied by the policy you pass. This gives you a way to further
      -- restrict the permissions for the resulting temporary security credentials.
      -- These policies and any applicable resource-based policies are evaluated
      -- when calls to AWS are made using the temporary security credentials.
    , arrRoleArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the role that the caller is assuming.
    , arrRoleSessionName :: !Text
      -- ^ An identifier for the assumed role session. The session name is included as
      -- part of the AssumedRoleUser.
    } deriving (Eq, Show, Generic)

instance ToQuery AssumeRole

instance AWSRequest AssumeRole where
    type Er AssumeRole = STSError
    type Rs AssumeRole = AssumeRoleResponse
    request = getQuery service "AssumeRole"

data AssumeRoleResponse = AssumeRoleResponse
    { arrrsAssumedRoleUser :: Maybe AssumedRoleUser
      -- ^ The Amazon Resource Name (ARN) and the assumed role ID, which are
      -- identifiers that you can use to refer to the resulting temporary security
      -- credentials. For example, you can reference these credentials as a
      -- principal in a resource-based policy by using the ARN or assumed role ID.
      -- The ARN and ID include the RoleSessionName that you specified when you
      -- called AssumeRole.
    , arrrsCredentials :: Maybe Credentials
      -- ^ The temporary security credentials, which include an access key ID, a
      -- secret access key, and a security token.
    , arrrsPackedPolicySize :: Maybe Int
      -- ^ A percentage value that indicates the size of the policy in packed form.
      -- The service rejects any policy with a packed size greater than 100 percent,
      -- which means the policy exceeded the allowed space.
    } deriving (Eq, Show, Generic)

instance FromXML AssumeRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AssumeRoleResponse"
        :| ["AssumeRoleResult"]
