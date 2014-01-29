{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.GetFederationToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) for a federated user. A
-- typical use is in a proxy application that is getting temporary security
-- credentials on behalf of distributed applications inside a corporate
-- network. Because you must call the GetFederationToken action using the
-- long-term security credentials of an IAM user, this call is appropriate in
-- contexts where those credentials can be safely stored, usually in a
-- server-based application. Note: Do not use this call in mobile applications
-- or client-based web applications that directly get temporary security
-- credentials. For those types of applications, use
-- AssumeRoleWithWebIdentity. The GetFederationToken action must be called by
-- using the long-term AWS security credentials of the AWS account or an IAM
-- user. Credentials that are created by IAM users are valid for the specified
-- duration, between 900 seconds (15 minutes) and 129600 seconds (36 hours);
-- credentials that are created by using account credentials have a maximum
-- duration of 3600 seconds (1 hour). Optionally, you can pass an AWS IAM
-- access policy to this operation. The temporary security credentials that
-- are returned by the operation have the permissions that are associated with
-- the entity that is making the GetFederationToken call, except for any
-- permissions explicitly denied by the policy you pass. This gives you a way
-- to further restrict the permissions for the resulting temporary security
-- credentials. These policies and any applicable resource-based policies are
-- evaluated when calls to AWS are made using the temporary security
-- credentials. For more information about how permissions work, see
-- Controlling Permissions in Temporary Credentials in Using Temporary
-- Security Credentials. For information about using GetFederationToken to
-- create temporary security credentials, see Creating Temporary Credentials
-- to Enable Access for Federated Users in Using Temporary Security
-- Credentials. https://sts.amazonaws.com/ ?Version=2011-06-15
-- &Action=GetFederationToken &Name=Bob
-- &Policy=%7B%22Version%22%3A%222012-10-17%22%2C%22Statement%22%3A%5B%7B%22Sid%22%3A%22Stmt1%22%2C%22Effect%22%
-- 3A%22Allow%22%2C%22Action%22%3A%22s3%3A*%22%2C%22Resource%22%3A%22*%22%7D
-- %5D%7D &DurationSeconds=3600 &AUTHPARAMS 2011-06-15/">
-- AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
-- LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
-- QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
-- 9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
-- +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-15T23:28:33.359Z
-- AKIAIOSFODNN7EXAMPLE arn:aws:sts::123456789012:federated-user/Bob
-- 123456789012:Bob 6 c6104cbe-af31-11e0-8154-cbc7ccf896c7.
module Network.AWS.STS.GetFederationToken where

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
getFederationToken :: Text
                   -> GetFederationToken
getFederationToken p1 = undefined $ GetFederationToken
    { gftrName = p1
    , gftrDurationSeconds = Nothing
    , gftrPolicy = Nothing
    }

data GetFederationToken = GetFederationToken
    { gftrDurationSeconds :: Maybe Int
      -- ^ The duration, in seconds, that the session should last. Acceptable
      -- durations for federation sessions range from 900 seconds (15 minutes) to
      -- 129600 seconds (36 hours), with 43200 seconds (12 hours) as the default.
      -- Sessions for AWS account owners are restricted to a maximum of 3600 seconds
      -- (one hour). If the duration is longer than one hour, the session for AWS
      -- account owners defaults to one hour.
    , gftrName :: !Text
      -- ^ The name of the federated user. The name is used as an identifier for the
      -- temporary security credentials (such as Bob). For example, you can
      -- reference the federated user name in a resource-based policy, such as in an
      -- Amazon S3 bucket policy.
    , gftrPolicy :: Maybe Text
      -- ^ An AWS IAM policy in JSON format. By default, federated users have no
      -- permissions; they do not inherit any from the IAM user. When you specify a
      -- policy, the federated user's permissions are based on the specified policy
      -- and the IAM user's policy. If you don't specify a policy, federated users
      -- can only access AWS resources that explicitly allow those federated users
      -- in a resource policy, such as in an Amazon S3 bucket policy.
    } deriving (Eq, Show, Generic)

instance ToQuery GetFederationToken

instance AWSRequest GetFederationToken where
    type Er GetFederationToken = STSError
    type Rs GetFederationToken = GetFederationTokenResponse
    request = getQuery service "GetFederationToken"

data GetFederationTokenResponse = GetFederationTokenResponse
    { gftrrsCredentials :: Maybe Credentials
      -- ^ Credentials for the service API authentication.
    , gftrrsFederatedUser :: Maybe FederatedUser
      -- ^ Identifiers for the federated user associated with the credentials (such as
      -- arn:aws:sts::123456789012:federated-user/Bob or 123456789012:Bob). You can
      -- use the federated user's ARN in your resource policies like in an Amazon S3
      -- bucket policy.
    , gftrrsPackedPolicySize :: Maybe Int
      -- ^ A percentage value indicating the size of the policy in packed form. The
      -- service rejects policies for which the packed size is greater than 100
      -- percent of the allowed value.
    } deriving (Eq, Show, Generic)

instance FromXML GetFederationTokenResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetFederationTokenResponse"
        :| ["GetFederationTokenResult"]
