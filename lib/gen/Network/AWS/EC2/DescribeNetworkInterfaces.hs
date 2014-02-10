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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { dnidDryRun :: Maybe Bool
    , dnidFilters :: [Filter]
    , dnidNetworkInterfaceIds :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkInterfaces

instance AWSRequest DescribeNetworkInterfaces where
    type Er DescribeNetworkInterfaces = EC2Error
    type Rs DescribeNetworkInterfaces = DescribeNetworkInterfacesResponse
    request  = postQuery service "DescribeNetworkInterfaces"
    response = responseXML

data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse
    { dnidrNetworkInterfaceSet :: [NetworkInterface]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkInterfacesResponse where
    fromXMLOptions = xmlOptions
