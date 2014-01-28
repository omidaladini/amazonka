{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target. By default, a gateway does
-- not have CHAP enabled; however, for added security, you might use it. When
-- you update CHAP credentials, all existing connections on the target are
-- closed and initiators must reconnect with the new credentials. Example
-- Request The following example shows a request that updates CHAP credentials
-- for an iSCSI target. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 161 { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" }.
module Network.AWS.StorageGateway.UpdateChapCredentials where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

data UpdateChapCredentials = UpdateChapCredentials
    { ucciInitiatorName :: !Text
      -- ^ The iSCSI initiator that connects to the target.
    , ucciSecretToAuthenticateInitiator :: !Text
      -- ^ The secret key that the initiator (e.g. Windows client) must provide to
      -- participate in mutual CHAP with the target.
    , ucciSecretToAuthenticateTarget :: Maybe Text
      -- ^ The secret key that the target must provide to participate in mutual CHAP
      -- with the initiator (e.g. Windows client).
    , ucciTargetARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
      -- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
      -- for specified VolumeARN.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateChapCredentials

instance AWSRequest UpdateChapCredentials where
    type Er UpdateChapCredentials = StorageGatewayError
    type Rs UpdateChapCredentials = UpdateChapCredentialsResponse
    request  = getJSON service
    response = responseJSON

data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse
    { uccirsInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target. This is the same initiator
      -- name specified in the request.
    , uccirsTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the target. This is the same target
      -- specified in the request.
    } deriving (Eq, Show, Generic)

instance FromJSON UpdateChapCredentialsResponse
