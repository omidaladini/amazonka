{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified instance. You must stop an instance before you can
-- delete it. For more information, see Deleting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.DeleteInstance where

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
deleteInstance :: Text
               -> DeleteInstance
deleteInstance p1 = DeleteInstance
    { dirInstanceId = p1
    , dirDeleteElasticIp = Nothing
    , dirDeleteVolumes = Nothing
    }

data DeleteInstance = DeleteInstance
    { dirDeleteElasticIp :: Maybe Bool
      -- ^ Whether to delete the instance Elastic IP address.
    , dirDeleteVolumes :: Maybe Bool
      -- ^ Whether to delete the instance Amazon EBS volumes.
    , dirInstanceId :: !Text
      -- ^ The instance ID.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteInstance where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteInstance where
    type Er DeleteInstance = OpsWorksError
    type Rs DeleteInstance = DeleteInstanceResponse
    request  = getJSON service
    response = responseJSON

data DeleteInstanceResponse = DeleteInstanceResponse
    deriving (Eq, Show, Generic)

instance FromJSON DeleteInstanceResponse where
    fromJSON = genericFromJSON jsonOptions

