{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the cluster
-- steps, see ListSteps.
module Network.AWS.EMR.DescribeCluster where

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

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

-- | Convenience method utilising default fields where applicable.
describeCluster :: AWS (Either EMRError DescribeClusterResponse)
describeCluster = undefined $ DescribeCluster
    { dciClusterId = Nothing
    }

data DescribeCluster = DescribeCluster
    { dciClusterId :: Maybe Text
      -- ^ The identifier of the cluster to describe.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCluster

instance AWSRequest DescribeCluster where
    type Er DescribeCluster = EMRError
    type Rs DescribeCluster = DescribeClusterResponse
    request  = getJSON service
    response = responseJSON

data DescribeClusterResponse = DescribeClusterResponse
    { dcirsCluster :: Maybe Cluster
      -- ^ This output contains the details for the requested cluster.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeClusterResponse
