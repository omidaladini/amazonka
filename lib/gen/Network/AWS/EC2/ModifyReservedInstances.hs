{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyReservedInstances operation modifies the Availability Zone,
-- instance count, instance type, or network platform (EC2-Classic or EC2-VPC)
-- of your Reserved Instances.
module Network.AWS.EC2.ModifyReservedInstances where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ModifyReservedInstances = ModifyReservedInstances
    { mrirClientToken :: Maybe Text
      -- ^ A unique, case-sensitive, token you provide to ensure idempotency of your
      -- modification request.
    , mrirReservedInstancesIds :: [Text]
      -- ^ The IDs of the Reserved Instances to modify.
    , mrirTargetConfigurations :: [ReservedInstancesConfiguration]
      -- ^ The configuration settings for the Reserved Instances to modify.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyReservedInstances

instance AWSRequest ModifyReservedInstances where
    type Er ModifyReservedInstances = EC2Error
    type Rs ModifyReservedInstances = ModifyReservedInstancesResponse
    request = v2Query service GET "ModifyReservedInstances"

data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { mrirrsReservedInstancesModificationId :: Maybe Text
      -- ^ The unique ID for the submitted modification request.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyReservedInstancesResponse where
    fromXMLOptions = xmlOptions
