{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Assigns one of the stack's registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.AssignVolume where

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
assignVolume :: Text
             -> AssignVolume
assignVolume p1 = undefined $ AssignVolume
    { avrVolumeId = p1
    , avrInstanceId = Nothing
    }

data AssignVolume = AssignVolume
    { avrInstanceId :: Maybe Text
      -- ^ The instance ID.
    , avrVolumeId :: !Text
      -- ^ The volume ID.
    } deriving (Eq, Show, Generic)

instance ToJSON AssignVolume

instance AWSRequest AssignVolume where
    type Er AssignVolume = OpsWorksError
    type Rs AssignVolume = AssignVolumeResponse
    request  = getJSON service
    response = responseJSON

data AssignVolumeResponse = AssignVolumeResponse
    deriving (Eq, Show, Generic)

instance FromJSON AssignVolumeResponse
