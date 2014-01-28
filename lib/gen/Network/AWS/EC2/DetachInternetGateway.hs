{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches an Internet gateway from a VPC, disabling connectivity between the
-- Internet and the VPC. The VPC must not contain any running instances with
-- elastic IP addresses. For more information about your VPC and Internet
-- gateway, go to Amazon Virtual Private Cloud User Guide. For more
-- information about Amazon Virtual Private Cloud and Internet gateways, go to
-- the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.DetachInternetGateway where

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

data DetachInternetGateway = DetachInternetGateway
    { digrDryRun :: Maybe Bool
    , digrInternetGatewayId :: !Text
      -- ^ The ID of the Internet gateway to detach.
    , digrVpcId :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery DetachInternetGateway

instance AWSRequest DetachInternetGateway where
    type Er DetachInternetGateway = EC2Error
    type Rs DetachInternetGateway = DetachInternetGatewayResponse
    request = getQuery service "DetachInternetGateway"

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML DetachInternetGatewayResponse where
    fromXMLOptions = xmlOptions