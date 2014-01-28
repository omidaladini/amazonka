{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, go to the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.AttachInternetGateway where

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

data AttachInternetGateway = AttachInternetGateway
    { aigrDryRun :: Maybe Bool
    , aigrInternetGatewayId :: !Text
      -- ^ The ID of the Internet gateway to attach.
    , aigrVpcId :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachInternetGateway

instance AWSRequest AttachInternetGateway where
    type Er AttachInternetGateway = EC2Error
    type Rs AttachInternetGateway = AttachInternetGatewayResponse
    request = getQuery service "AttachInternetGateway"

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML AttachInternetGatewayResponse where
    fromXMLOptions = xmlOptions
