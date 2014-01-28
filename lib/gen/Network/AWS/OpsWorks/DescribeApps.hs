{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of apps. You must specify at
-- least one of the parameters. Required Permissions: To use this action, an
-- IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeApps where

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

data DescribeApps = DescribeApps
    { dasAppIds :: [Text]
      -- ^ An array of app IDs for the apps to be described. If you use this
      -- parameter, DescribeApps returns a description of the specified apps.
      -- Otherwise, it returns a description of every app.
    , dasStackId :: Maybe Text
      -- ^ The app stack ID. If you use this parameter, DescribeApps returns a
      -- description of the apps in the specified stack.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeApps

instance AWSRequest DescribeApps where
    type Er DescribeApps = OpsWorksError
    type Rs DescribeApps = DescribeAppsResponse
    request  = getJSON service
    response = responseJSON

data DescribeAppsResponse = DescribeAppsResponse
    { dasrsApps :: [App]
      -- ^ An array of App objects that describe the specified apps.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeAppsResponse
