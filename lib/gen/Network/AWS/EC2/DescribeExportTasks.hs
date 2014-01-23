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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeExportTasks = DescribeExportTasks
    { detrExportTaskIds :: [Text]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeExportTasks

instance AWSRequest DescribeExportTasks where
    type Er DescribeExportTasks = EC2Error
    type Rs DescribeExportTasks = DescribeExportTasksResponse
    request = v2Query service GET "DescribeExportTasks"

data DescribeExportTasksResponse = DescribeExportTasksResponse
    { detrrsExportTasks :: [ExportTask]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeExportTasksResponse where
    fromXMLOptions = xmlOptions
