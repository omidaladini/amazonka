{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials for users who have been
-- authenticated via a SAML authentication response. This operation provides a
-- mechanism for tying an enterprise identity store or directory to role-based
-- AWS access without user-specific credentials or configuration. The
-- temporary security credentials returned by this operation consist of an
-- access key ID, a secret access key, and a security token. Applications can
-- use these temporary security credentials to sign calls to AWS services. The
-- credentials are valid for the duration that you specified when calling
-- AssumeRoleWithSAML, which can be up to 3600 seconds (1 hour) or until the
-- time specified in the SAML authentication response's NotOnOrAfter value,
-- whichever is shorter. The maximum duration for a session is 1 hour, and the
-- minimum duration is 15 minutes, even if values outside this range are
-- specified. Optionally, you can pass an AWS IAM access policy to this
-- operation. The temporary security credentials that are returned by the
-- operation have the permissions that are associated with the access policy
-- of the role being assumed, except for any permissions explicitly denied by
-- the policy you pass. This gives you a way to further restrict the
-- permissions for the resulting temporary security credentials. These
-- policies and any applicable resource-based policies are evaluated when
-- calls to AWS are made using the temporary security credentials. Before your
-- application can call AssumeRoleWithSAML, you must configure your SAML
-- identity provider (IdP) to issue the claims required by AWS. Additionally,
-- you must use AWS Identity and Access Management (AWS IAM) to create a SAML
-- provider entity in your AWS account that represents your identity provider,
-- and create an AWS IAM role that specifies this SAML provider in its trust
-- policy. Calling AssumeRoleWithSAML does not require the use of AWS security
-- credentials. The identity of the caller is validated by using keys in the
-- metadata document that is uploaded for the SAML provider entity for your
-- identity provider. For more information, see the following resources:
-- Creating Temporary Security Credentials for SAML Federation in the Using
-- Temporary Security Credentials guide. SAML Providers in the Using IAM
-- guide. Configuring a Relying Party and Claims in the Using IAM guide.
-- Creating a Role for SAML-Based Federation in the Using IAM guide.
module Network.AWS.STS.AssumeRoleWithSAML where

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

data AssumeRoleWithSAML = AssumeRoleWithSAML
    { arwsamlrDurationSeconds :: Maybe Int
      -- ^ The duration, in seconds, of the role session. The value can range from 900
      -- seconds (15 minutes) to 3600 seconds (1 hour). By default, the value is set
      -- to 3600 seconds. An expiration can also be specified in the SAML
      -- authentication response's NotOnOrAfter value. The actual expiration time is
      -- whichever value is shorter. The maximum duration for a session is 1 hour,
      -- and the minimum duration is 15 minutes, even if values outside this range
      -- are specified.
    , arwsamlrPolicy :: Maybe Text
      -- ^ An AWS IAM policy in JSON format. The temporary security credentials that
      -- are returned by this operation have the permissions that are associated
      -- with the access policy of the role being assumed, except for any
      -- permissions explicitly denied by the policy you pass. These policies and
      -- any applicable resource-based policies are evaluated when calls to AWS are
      -- made using the temporary security credentials. The policy must be 2048
      -- bytes or shorter, and its packed size must be less than 450 bytes.
    , arwsamlrPrincipalArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider in AWS IAM that
      -- describes the IdP.
    , arwsamlrRoleArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the role that the caller is assuming.
    , arwsamlrSAMLAssertion :: !Text
      -- ^ The base-64 encoded SAML authentication response provided by the IdP. For
      -- more information, see Configuring a Relying Party and Adding Claims in the
      -- Using IAM guide.
    } deriving (Eq, Show, Generic)

instance ToQuery AssumeRoleWithSAML

instance AWSRequest AssumeRoleWithSAML where
    type Er AssumeRoleWithSAML = STSError
    type Rs AssumeRoleWithSAML = AssumeRoleWithSAMLResponse
    request = getQuery service "AssumeRoleWithSAML"

data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse
    { arwsamlrrsAssumedRoleUser :: Maybe AssumedRoleUser
      -- ^ The identifiers for the temporary security credentials that the operation
      -- returns.
    , arwsamlrrsCredentials :: Maybe Credentials
      -- ^ AWS credentials for API authentication.
    , arwsamlrrsPackedPolicySize :: Maybe Int
      -- ^ A percentage value that indicates the size of the policy in packed form.
      -- The service rejects any policy with a packed size greater than 100 percent,
      -- which means the policy exceeded the allowed space.
    } deriving (Eq, Show, Generic)

instance FromXML AssumeRoleWithSAMLResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AssumeRoleWithSAMLResponse"
        :| ["AssumeRoleWithSAMLResult"]
