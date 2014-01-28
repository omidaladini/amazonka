{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a Spot Instance request. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current spot instance requests. For
-- conceptual information about Spot Instances, refer to the Amazon Elastic
-- Compute Cloud Developer Guide or Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.RequestSpotInstances where

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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data RequestSpotInstances = RequestSpotInstances
    { rsirAvailabilityZoneGroup :: Maybe Text
      -- ^ Specifies the Availability Zone group. When specifying the same
      -- Availability Zone group for all Spot Instance requests, all Spot Instances
      -- are launched in the same Availability Zone.
    , rsirDryRun :: Maybe Bool
    , rsirInstanceCount :: Maybe Int
      -- ^ Specifies the maximum number of Spot Instances to launch.
    , rsirLaunchGroup :: Maybe Text
      -- ^ Specifies the instance launch group. Launch groups are Spot Instances that
      -- launch and terminate together.
    , rsirLaunchSpecification :: Maybe LaunchSpecification
      -- ^ Specifies additional launch instance information.
    , rsirSpotPrice :: !Text
      -- ^ Specifies the maximum hourly price for any Spot Instance launched to
      -- fulfill the request.
    , rsirType :: Maybe SpotInstanceType
      -- ^ Specifies the Spot Instance type.
    , rsirValidFrom :: Maybe UTCTime
      -- ^ Defines the start date of the request. If this is a one-time request, the
      -- request becomes active at this date and time and remains active until all
      -- instances launch, the request expires, or the request is canceled. If the
      -- request is persistent, the request becomes active at this date and time and
      -- remains active until it expires or is canceled.
    , rsirValidUntil :: Maybe UTCTime
      -- ^ End date of the request. If this is a one-time request, the request remains
      -- active until all instances launch, the request is canceled, or this date is
      -- reached. If the request is persistent, it remains active until it is
      -- canceled or this date and time is reached.
    } deriving (Eq, Show, Generic)

instance ToQuery RequestSpotInstances

instance AWSRequest RequestSpotInstances where
    type Er RequestSpotInstances = EC2Error
    type Rs RequestSpotInstances = RequestSpotInstancesResponse
    request = getQuery service "RequestSpotInstances"

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { rsirrsSpotInstanceRequests :: [SpotInstanceRequest]
      -- ^ Contains a list of Spot Instance requests.
    } deriving (Eq, Show, Generic)

instance FromXML RequestSpotInstancesResponse where
    fromXMLOptions = xmlOptions
