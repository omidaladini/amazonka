{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeElasticIps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Elastic IP addresses. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeElasticIps where

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

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeElasticIps :: DescribeElasticIps
describeElasticIps = DescribeElasticIps
    { deitInstanceId = Nothing
    , deitIps = []
    , deitStackId = Nothing
    }

data DescribeElasticIps = DescribeElasticIps
    { deitInstanceId :: Maybe Text
      -- ^ The instance ID. If you include this parameter, DescribeElasticIps returns
      -- a description of the Elastic IP addresses associated with the specified
      -- instance.
    , deitIps :: [Text]
      -- ^ An array of Elastic IP addresses to be described. If you include this
      -- parameter, DescribeElasticIps returns a description of the specified
      -- Elastic IP addresses. Otherwise, it returns a description of every Elastic
      -- IP address.
    , deitStackId :: Maybe Text
      -- ^ A stack ID. If you include this parameter, DescribeElasticIps returns a
      -- description of the Elastic IP addresses that are registered with the
      -- specified stack.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeElasticIps

instance AWSRequest DescribeElasticIps where
    type Er DescribeElasticIps = OpsWorksError
    type Rs DescribeElasticIps = DescribeElasticIpsResponse
    request  = getJSON service
    response = responseJSON

data DescribeElasticIpsResponse = DescribeElasticIpsResponse
    { deitrsElasticIps :: [ElasticIp]
      -- ^ An ElasticIps object that describes the specified Elastic IP addresses.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeElasticIpsResponse
