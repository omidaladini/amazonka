{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describe specified users. Required Permissions: To use this action, an IAM
-- user must have an attached policy that explicitly grants permissions. For
-- more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeUserProfiles where

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
describeUserProfiles :: DescribeUserProfiles
describeUserProfiles = DescribeUserProfiles
    { duprIamUserArns = []
    }

data DescribeUserProfiles = DescribeUserProfiles
    { duprIamUserArns :: [Text]
      -- ^ An array of IAM user ARNs that identify the users to be described.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeUserProfiles

instance AWSRequest DescribeUserProfiles where
    type Er DescribeUserProfiles = OpsWorksError
    type Rs DescribeUserProfiles = DescribeUserProfilesResponse
    request  = getJSON service
    response = responseJSON

data DescribeUserProfilesResponse = DescribeUserProfilesResponse
    { duprrsUserProfiles :: [UserProfile]
      -- ^ A Users object that describes the specified users.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeUserProfilesResponse
