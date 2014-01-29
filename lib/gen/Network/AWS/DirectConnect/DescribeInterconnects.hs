{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of interconnects owned by the AWS account. If an
-- interconnect ID is provided, it will only return this particular
-- interconnect.
module Network.AWS.DirectConnect.DescribeInterconnects where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DirectConnect.Service
import Network.AWS.DirectConnect.Types

-- | Convenience method utilising default fields where applicable.
describeInterconnects :: AWS (Either DirectConnectError DescribeInterconnectsResponse)
describeInterconnects = undefined $ DescribeInterconnects
    { dirInterconnectId = Nothing
    }

data DescribeInterconnects = DescribeInterconnects
    { dirInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeInterconnects

instance AWSRequest DescribeInterconnects where
    type Er DescribeInterconnects = DirectConnectError
    type Rs DescribeInterconnects = DescribeInterconnectsResponse
    request  = getJSON service
    response = responseJSON

data DescribeInterconnectsResponse = DescribeInterconnectsResponse
    { dirrsInterconnects :: [Interconnect]
      -- ^ A list of interconnects.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeInterconnectsResponse
