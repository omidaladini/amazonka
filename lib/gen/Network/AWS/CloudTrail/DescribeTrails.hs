{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the settings for some or all trails associated with an account.
module Network.AWS.CloudTrail.DescribeTrails where

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

import Network.AWS.CloudTrail.Service
import Network.AWS.CloudTrail.Types

data DescribeTrails = DescribeTrails
    { dtrTrailNameList :: [Text]
      -- ^ The list of trails.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTrails

instance AWSRequest DescribeTrails where
    type Er DescribeTrails = CloudTrailError
    type Rs DescribeTrails = DescribeTrailsResponse
    request  = getJSON service
    response = responseJSON

data DescribeTrailsResponse = DescribeTrailsResponse
    { dtrrsTrailList :: [Trail]
      -- ^ The list of trails.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTrailsResponse