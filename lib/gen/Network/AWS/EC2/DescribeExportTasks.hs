{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeExportTasks
module Network.AWS.EC2.DescribeExportTasks where

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
describeExportTasks :: DescribeExportTasks
describeExportTasks = DescribeExportTasks
    { detrExportTaskIds = []
    }

data DescribeExportTasks = DescribeExportTasks
    { detrExportTaskIds :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeExportTasks

instance AWSRequest DescribeExportTasks where
    type Er DescribeExportTasks = EC2Error
    type Rs DescribeExportTasks = DescribeExportTasksResponse
    request = getQuery service "DescribeExportTasks"

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { detrrsExportTasks :: [ExportTask]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeExportTasksResponse where
    fromXMLOptions = xmlOptions
