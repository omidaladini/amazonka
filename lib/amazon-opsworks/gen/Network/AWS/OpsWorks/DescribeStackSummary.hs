{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the number of layers and apps in a specified stack, and the
-- number of instances in each state, such as running_setup or online.
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeStackSummary where

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
describeStackSummary :: Text
                     -> DescribeStackSummary
describeStackSummary p1 = DescribeStackSummary
    { dssrStackId = p1
    }

data DescribeStackSummary = DescribeStackSummary
    { dssrStackId :: !Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeStackSummary where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeStackSummary where
    type Er DescribeStackSummary = OpsWorksError
    type Rs DescribeStackSummary = DescribeStackSummaryResponse
    request  = getJSON service
    response = responseJSON

data DescribeStackSummaryResponse = DescribeStackSummaryResponse
    { dssrrsStackSummary :: Maybe StackSummary
      -- ^ A StackSummary object that contains the results.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeStackSummaryResponse where
    fromJSON = genericFromJSON jsonOptions

