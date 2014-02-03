{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeAccountAttributes
module Network.AWS.EC2.DescribeAccountAttributes where

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
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes
    { daarAttributeNames = []
    , daarDryRun = Nothing
    }

data DescribeAccountAttributes = DescribeAccountAttributes
    { daarAttributeNames :: [AccountAttributeName]
    , daarDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAccountAttributes

instance AWSRequest DescribeAccountAttributes where
    type Er DescribeAccountAttributes = EC2Error
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResponse
    request = getQuery service "DescribeAccountAttributes"

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { daarrsAccountAttributes :: [AccountAttribute]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAccountAttributesResponse where
    fromXMLOptions = xmlOptions
