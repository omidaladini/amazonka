{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Auto Scaling group if the group has no instances and
-- no scaling activities in progress. To remove all instances before calling
-- DeleteAutoScalingGroup, you can call UpdateAutoScalingGroup to set the
-- minimum and maximum size of the AutoScalingGroup to zero.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ForceDelete=true &Version=2011-01-01 &Action=DeleteAutoScalingGroup
-- &AUTHPARAMS 70a76d42-9665-11e2-9fdf-211deEXAMPLE.
module Network.AWS.AutoScaling.DeleteAutoScalingGroup where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields where applicable.
deleteAutoScalingGroup :: ResourceName
                       -> AWS (Either AutoScalingError DeleteAutoScalingGroupResponse)
deleteAutoScalingGroup p1 = undefined $ DeleteAutoScalingGroup
    { dasgtAutoScalingGroupName = p1
    , dasgtForceDelete = Nothing
    }

data DeleteAutoScalingGroup = DeleteAutoScalingGroup
    { dasgtAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group to delete.
    , dasgtForceDelete :: Maybe Bool
      -- ^ Starting with API version 2011-01-01, specifies that the Auto Scaling group
      -- will be deleted along with all instances associated with the group, without
      -- waiting for all instances to be terminated.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteAutoScalingGroup

instance AWSRequest DeleteAutoScalingGroup where
    type Er DeleteAutoScalingGroup = AutoScalingError
    type Rs DeleteAutoScalingGroup = DeleteAutoScalingGroupResponse
    request = getQuery service "DeleteAutoScalingGroup"

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteAutoScalingGroupResponse"
        :| ["DeleteAutoScalingGroupResult"]
