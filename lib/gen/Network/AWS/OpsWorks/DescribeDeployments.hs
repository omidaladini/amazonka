{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of deployments. You must specify
-- at least one of the parameters. Required Permissions: To use this action,
-- an IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeDeployments where

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

-- | Convenience method utilising default fields where applicable.
describeDeployments :: AWS (Either OpsWorksError DescribeDeploymentsResponse)
describeDeployments = undefined $ DescribeDeployments
    { ddrAppId = Nothing
    , ddrDeploymentIds = []
    , ddrStackId = Nothing
    }

data DescribeDeployments = DescribeDeployments
    { ddrAppId :: Maybe Text
      -- ^ The app ID. If you include this parameter, DescribeDeployments returns a
      -- description of the commands associated with the specified app.
    , ddrDeploymentIds :: [Text]
      -- ^ An array of deployment IDs to be described. If you include this parameter,
      -- DescribeDeployments returns a description of the specified deployments.
      -- Otherwise, it returns a description of every deployment.
    , ddrStackId :: Maybe Text
      -- ^ The stack ID. If you include this parameter, DescribeDeployments returns a
      -- description of the commands associated with the specified stack.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeDeployments

instance AWSRequest DescribeDeployments where
    type Er DescribeDeployments = OpsWorksError
    type Rs DescribeDeployments = DescribeDeploymentsResponse
    request  = getJSON service
    response = responseJSON

data DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { ddrrsDeployments :: [Deployment]
      -- ^ An array of Deployment objects that describe the deployments.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeDeploymentsResponse
