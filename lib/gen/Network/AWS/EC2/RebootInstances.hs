{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RebootInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RebootInstances operation requests a reboot of one or more instances.
-- This operation is asynchronous; it only queues a request to reboot the
-- specified instance(s). The operation will succeed if the instances are
-- valid and belong to the user. Requests to reboot terminated instances are
-- ignored.
module Network.AWS.EC2.RebootInstances where

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

data RebootInstances = RebootInstances
    { ritDryRun :: Maybe Bool
    , ritInstanceIds :: [Text]
      -- ^ The list of instances to terminate.
    } deriving (Eq, Show, Generic)

instance ToQuery RebootInstances

instance AWSRequest RebootInstances where
    type Er RebootInstances = EC2Error
    type Rs RebootInstances = RebootInstancesResponse
    request = getQuery service "RebootInstances"

data RebootInstancesResponse = RebootInstancesResponse
    deriving (Eq, Show, Generic)

instance FromXML RebootInstancesResponse where
    fromXMLOptions = xmlOptions
