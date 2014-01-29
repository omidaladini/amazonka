{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new route table within a VPC. After you create a new route table,
-- you can add routes and associate the table with a subnet. For more
-- information about route tables, go to Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
module Network.AWS.EC2.CreateRouteTable where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createRouteTable :: Text
                 -> CreateRouteTable
createRouteTable p1 = undefined $ CreateRouteTable
    { crtrVpcId = p1
    , crtrDryRun = Nothing
    }

data CreateRouteTable = CreateRouteTable
    { crtrDryRun :: Maybe Bool
    , crtrVpcId :: !Text
      -- ^ The ID of the VPC where the route table will be created.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateRouteTable

instance AWSRequest CreateRouteTable where
    type Er CreateRouteTable = EC2Error
    type Rs CreateRouteTable = CreateRouteTableResponse
    request = getQuery service "CreateRouteTable"

data CreateRouteTableResponse = CreateRouteTableResponse
    { crtrrsRouteTable :: Maybe RouteTable
    } deriving (Eq, Show, Generic)

instance FromXML CreateRouteTableResponse where
    fromXMLOptions = xmlOptions
