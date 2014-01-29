{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeNetworkInterfaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeNetworkInterfaces
module Network.AWS.EC2.DescribeNetworkInterfaces where

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
describeNetworkInterfaces :: AWS (Either EC2Error DescribeNetworkInterfacesResponse)
describeNetworkInterfaces = undefined $ DescribeNetworkInterfaces
    { dnisDryRun = Nothing
    , dnisFilters = []
    , dnisNetworkInterfaceIds = []
    }

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { dnisDryRun :: Maybe Bool
    , dnisFilters :: [Filter]
    , dnisNetworkInterfaceIds :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkInterfaces

instance AWSRequest DescribeNetworkInterfaces where
    type Er DescribeNetworkInterfaces = EC2Error
    type Rs DescribeNetworkInterfaces = DescribeNetworkInterfacesResponse
    request = getQuery service "DescribeNetworkInterfaces"

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { dnisrsNetworkInterfaces :: [NetworkInterface]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkInterfacesResponse where
    fromXMLOptions = xmlOptions
