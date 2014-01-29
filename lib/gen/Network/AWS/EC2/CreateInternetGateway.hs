{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Internet gateway in your AWS account. After creating the
-- Internet gateway, you then attach it to a VPC using AttachInternetGateway.
-- For more information about your VPC and Internet gateway, go to Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateInternetGateway where

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
createInternetGateway :: AWS (Either EC2Error CreateInternetGatewayResponse)
createInternetGateway = undefined $ CreateInternetGateway
    { cigrDryRun = Nothing
    }

data CreateInternetGateway = CreateInternetGateway
    { cigrDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery CreateInternetGateway

instance AWSRequest CreateInternetGateway where
    type Er CreateInternetGateway = EC2Error
    type Rs CreateInternetGateway = CreateInternetGatewayResponse
    request = getQuery service "CreateInternetGateway"

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { cigrrsInternetGateway :: Maybe InternetGateway
    } deriving (Eq, Show, Generic)

instance FromXML CreateInternetGatewayResponse where
    fromXMLOptions = xmlOptions
