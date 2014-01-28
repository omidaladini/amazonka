{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Internet gateway from your AWS account. The gateway must not be
-- attached to a VPC. For more information about your VPC and Internet
-- gateway, go to Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.DeleteInternetGateway where

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

data DeleteInternetGateway = DeleteInternetGateway
    { digtDryRun :: Maybe Bool
    , digtInternetGatewayId :: !Text
      -- ^ The ID of the Internet gateway to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteInternetGateway

instance AWSRequest DeleteInternetGateway where
    type Er DeleteInternetGateway = EC2Error
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse
    request = getQuery service "DeleteInternetGateway"

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteInternetGatewayResponse where
    fromXMLOptions = xmlOptions
