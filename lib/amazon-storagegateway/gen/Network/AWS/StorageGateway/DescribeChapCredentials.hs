{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns an array of Challenge-Handshake Authentication
-- Protocol (CHAP) credentials information for a specified iSCSI target, one
-- for each target-initiator pair. Example Request The following example shows
-- a request that returns the CHAP credentials of an iSCSI target. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 235 {
-- "ChapCredentials": { "TargetName": "iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } }.
module Network.AWS.StorageGateway.DescribeChapCredentials where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeChapCredentials :: Text
                        -> DescribeChapCredentials
describeChapCredentials p1 = DescribeChapCredentials
    { dcciTargetARN = p1
    }

data DescribeChapCredentials = DescribeChapCredentials
    { dcciTargetARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
      -- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
      -- for specified VolumeARN.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeChapCredentials where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeChapCredentials where
    type Er DescribeChapCredentials = StorageGatewayError
    type Rs DescribeChapCredentials = DescribeChapCredentialsResponse
    request  = getJSON service
    response = responseJSON

data DescribeChapCredentialsResponse = DescribeChapCredentialsResponse
    { dccirsChapCredentials :: [ChapInfo]
      -- ^ An array of ChapInfo objects that represent CHAP credentials. Each object
      -- in the array contains CHAP credential information for one target-initiator
      -- pair. If no CHAP credentials are set, an empty array is returned. CHAP
      -- credential information is provided in a JSON object with the following
      -- fields: InitiatorName: The iSCSI initiator that connects to the target.
      -- SecretToAuthenticateInitiator: The secret key that the initiator (e.g.
      -- Windows client) must provide to participate in mutual CHAP with the target.
      -- SecretToAuthenticateTarget: The secret key that the target must provide to
      -- participate in mutual CHAP with the initiator (e.g. Windows client).
      -- TargetARN: The Amazon Resource Name (ARN) of the storage volume.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeChapCredentialsResponse where
    fromJSON = genericFromJSON jsonOptions

