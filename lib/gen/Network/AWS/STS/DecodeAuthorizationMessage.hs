{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Decodes additional information about the authorization status of a request
-- from an encoded message returned in response to an AWS request. For
-- example, if a user is not authorized to perform an action that he or she
-- has requested, the request returns a Client.UnauthorizedOperation response
-- (an HTTP 403 response). Some AWS actions additionally return an encoded
-- message that can provide details about this authorization failure. Only
-- certain AWS actions return an encoded authorization message. The
-- documentation for an individual action indicates whether that action
-- returns an encoded message in addition to returning an HTTP code. The
-- message is encoded because the details of the authorization status can
-- constitute privileged information that the user who requested the action
-- should not see. To decode an authorization status message, a user must be
-- granted permissions via an AWS IAM policy to request the
-- DecodeAuthorizationMessage (sts:DecodeAuthorizationMessage) action. The
-- decoded message includes the following type of information: Whether the
-- request was denied due to an explicit deny or due to the absence of an
-- explicit allow. For more information, see Determining Whether a Request is
-- Allowed or Denied in Using AWS IAM. The principal who made the request. The
-- requested action. The requested resource. The values of condition keys in
-- the context of the user's request. POST https://sts.amazonaws.com /
-- HTTP/1.1 Content-Type: application/x-www-form-urlencoded; charset=utf-8
-- Host: sts.amazonaws.com Content-Length: 1148 Expect: 100-continue
-- Connection: Keep-Alive Action=DecodeAuthorizationMessage &EncodedMessage=
-- &Version=2011-06-15 &AUTHPARAMS 6624a9ca-cd25-4f50-b2a5-7ba65bf07453 {
-- "allowed": "false", "explicitDeny": "false", "matchedStatements": "",
-- "failures": "", "context": { "principal": { "id": "AIDACKCEVSQ6C2EXAMPLE",
-- "name": "Bob", "arn": "arn:aws:iam::123456789012:user/Bob" }, "action":
-- "ec2:StopInstances", "resource":
-- "arn:aws:ec2:us-east-1:123456789012:instance/i-dd01c9bd", "conditions": [ {
-- "item": { "key": "ec2:Tenancy", "values": ["default"] }, { "item": { "key":
-- "ec2:ResourceTag/elasticbeanstalk:environment-name", "values":
-- ["Default-Environment"] } }, (Additional items ...) ] } }.
module Network.AWS.STS.DecodeAuthorizationMessage where

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
decodeAuthorizationMessage :: Text
                           -> DecodeAuthorizationMessage
decodeAuthorizationMessage p1 = undefined $ DecodeAuthorizationMessage
    { damrEncodedMessage = p1
    }

data DecodeAuthorizationMessage = DecodeAuthorizationMessage
    { damrEncodedMessage :: !Text
      -- ^ The encoded message that was returned with the response.
    } deriving (Eq, Show, Generic)

instance ToQuery DecodeAuthorizationMessage

instance AWSRequest DecodeAuthorizationMessage where
    type Er DecodeAuthorizationMessage = STSError
    type Rs DecodeAuthorizationMessage = DecodeAuthorizationMessageResponse
    request = getQuery service "DecodeAuthorizationMessage"

data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse
    { damrrsDecodedMessage :: Maybe Text
      -- ^ An XML document that contains the decoded message. For more information,
      -- see DecodeAuthorizationMessage.
    } deriving (Eq, Show, Generic)

instance FromXML DecodeAuthorizationMessageResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DecodeAuthorizationMessageResponse"
        :| ["DecodeAuthorizationMessageResult"]
