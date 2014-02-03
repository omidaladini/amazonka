{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.GetSessionToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary credentials for an AWS account or IAM user. The
-- credentials consist of an access key ID, a secret access key, and a
-- security token. Typically, you use GetSessionToken if you want use MFA to
-- protect programmatic calls to specific AWS APIs like Amazon EC2
-- StopInstances. MFA-enabled IAM users would need to call GetSessionToken and
-- submit an MFA code that is associated with their MFA device. Using the
-- temporary security credentials that are returned from the call, IAM users
-- can then make programmatic calls to APIs that require MFA authentication.
-- The GetSessionToken action must be called by using the long-term AWS
-- security credentials of the AWS account or an IAM user. Credentials that
-- are created by IAM users are valid for the duration that you specify,
-- between 900 seconds (15 minutes) and 129600 seconds (36 hours); credentials
-- that are created by using account credentials have a maximum duration of
-- 3600 seconds (1 hour). Optionally, you can pass an AWS IAM access policy to
-- this operation. The temporary security credentials that are returned by the
-- operation have the permissions that are associated with the entity that is
-- making the GetSessionToken call, except for any permissions explicitly
-- denied by the policy you pass. This gives you a way to further restrict the
-- permissions for the resulting temporary security credentials. These
-- policies and any applicable resource-based policies are evaluated when
-- calls to AWS are made using the temporary security credentials. For more
-- information about using GetSessionToken to create temporary credentials, go
-- to Creating Temporary Credentials to Enable Access for IAM Users in Using
-- IAM. https://sts.amazonaws.com/ ?Version=2011-06-15 &Action=GetSessionToken
-- &DurationSeconds=3600 &SerialNumber=YourMFADeviceSerialNumber
-- &TokenCode=123456 &AUTHPARAMS
-- AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/L
-- To6UDdyJwOOvEVPvLXCrrrUtdnniCEXAMPLE/IvU1dYUg2RVAJBanLiHb4IgRmpRV3z
-- rkuWJOgQs8IZZaIv2BXIa2R4OlgkBN9bkUDNCJiBeb/AXlzBBko7b15fjrBs2+cTQtp
-- Z3CYWFXG8C5zqx37wnOE49mRl/+OtkIKGO7fAE
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-11T19:55:29.611Z
-- AKIAIOSFODNN7EXAMPLE 58c5dbae-abef-11e0-8cfe-09039844ac7d.
module Network.AWS.STS.GetSessionToken where

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
getSessionToken :: GetSessionToken
getSessionToken = GetSessionToken
    { gstrDurationSeconds = Nothing
    , gstrSerialNumber = Nothing
    , gstrTokenCode = Nothing
    }

data GetSessionToken = GetSessionToken
    { gstrDurationSeconds :: Maybe Int
      -- ^ The duration, in seconds, that the credentials should remain valid.
      -- Acceptable durations for IAM user sessions range from 900 seconds (15
      -- minutes) to 129600 seconds (36 hours), with 43200 seconds (12 hours) as the
      -- default. Sessions for AWS account owners are restricted to a maximum of
      -- 3600 seconds (one hour). If the duration is longer than one hour, the
      -- session for AWS account owners defaults to one hour.
    , gstrSerialNumber :: Maybe Text
      -- ^ The identification number of the MFA device that is associated with the IAM
      -- user who is making the GetSessionToken call. Specify this value if the IAM
      -- user has a policy that requires MFA authentication. The value is either the
      -- serial number for a hardware device (such as GAHT12345678) or an Amazon
      -- Resource Name (ARN) for a virtual device (such as
      -- arn:aws:iam::123456789012:mfa/user). You can find the device for an IAM
      -- user by going to the AWS Management Console and viewing the user's security
      -- credentials.
    , gstrTokenCode :: Maybe Text
      -- ^ The value provided by the MFA device, if MFA is required. If any policy
      -- requires the IAM user to submit an MFA code, specify this value. If MFA
      -- authentication is required, and the user does not provide a code when
      -- requesting a set of temporary security credentials, the user will receive
      -- an "access denied" response when requesting resources that require MFA
      -- authentication.
    } deriving (Eq, Show, Generic)

instance ToQuery GetSessionToken

instance AWSRequest GetSessionToken where
    type Er GetSessionToken = STSError
    type Rs GetSessionToken = GetSessionTokenResponse
    request = getQuery service "GetSessionToken"

data GetSessionTokenResponse = GetSessionTokenResponse
    { gstrrsCredentials :: Maybe Credentials
      -- ^ The session credentials for API authentication.
    } deriving (Eq, Show, Generic)

instance FromXML GetSessionTokenResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetSessionTokenResponse"
        :| ["GetSessionTokenResult"]
