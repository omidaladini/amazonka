{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a route from a route table in a VPC. For more information about
-- route tables, go to Route Tables in the Amazon Virtual Private Cloud User
-- Guide.
module Network.AWS.EC2.DeleteRoute where

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

-- | Convenience method utilising default fields where applicable.
deleteRoute :: Text
            -> Text
            -> AWS (Either EC2Error DeleteRouteResponse)
deleteRoute p1 p2 = undefined $ DeleteRoute
    { drrDestinationCidrBlock = p1
    , drrRouteTableId = p2
    , drrDryRun = Nothing
    }

data DeleteRoute = DeleteRoute
    { drrDestinationCidrBlock :: !Text
      -- ^ The CIDR range for the route you want to delete. The value you specify must
      -- exactly match the CIDR for the route you want to delete.
    , drrDryRun :: Maybe Bool
    , drrRouteTableId :: !Text
      -- ^ The ID of the route table where the route will be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteRoute

instance AWSRequest DeleteRoute where
    type Er DeleteRoute = EC2Error
    type Rs DeleteRoute = DeleteRouteResponse
    request = getQuery service "DeleteRoute"

data DeleteRouteResponse = DeleteRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteRouteResponse where
    fromXMLOptions = xmlOptions
