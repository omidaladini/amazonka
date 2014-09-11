{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

-- | Describes one or more of your export tasks. Example This example describes
-- a single export task. https://ec2.amazonaws.com/?Action=DescribeExportTasks
-- &amp;exportTaskId.1=export-i-1234wxyz &amp;AUTHPARAMS
-- &lt;DescribeExportTasksResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;exportTaskSet&gt; &lt;item&gt;
-- &lt;exportTaskId&gt;export-i-1234wxyz&lt;/exportTaskId&gt;
-- &lt;description&gt;Example for docs&lt;/description&gt;
-- &lt;state&gt;active&lt;/state&gt;
-- &lt;statusMessage&gt;Running&lt;/statusMessage&gt; &lt;instanceExport&gt;
-- &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;targetEnvironment&gt;VMWare&lt;/targetEnvironment&gt;
-- &lt;/instanceExport&gt; &lt;exportToS3&gt;
-- &lt;diskImageFormat&gt;VMDK&lt;/diskImageFormat&gt;
-- &lt;containerFormat&gt;OVA&lt;/containerFormat&gt;
-- &lt;s3Bucket&gt;my-bucket-for-exported-vm&lt;/s3Bucket&gt;
-- &lt;s3Key&gt;my-exports/ export-i-1234wxyz .ova&lt;/s3Key&gt;
-- &lt;/exportToS3&gt; &lt;/item&gt; &lt;/exportTaskSet&gt; &lt;/
-- DescribeExportTasksResponse&gt;.
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Request
      DescribeExportTasks
    -- ** Request constructor
    , mkDescribeExportTasks
    -- ** Request lenses
    , detExportTaskIds

    -- * Response
    , DescribeExportTasksResponse
    -- ** Response constructor
    , mkDescribeExportTasksResponse
    -- ** Response lenses
    , detrExportTasks
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DescribeExportTasks = DescribeExportTasks
    { _detExportTaskIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeExportTasks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExportTaskIds ::@ @[Text]@
--
mkDescribeExportTasks :: DescribeExportTasks
mkDescribeExportTasks = DescribeExportTasks
    { _detExportTaskIds = mempty
    }

-- | One or more export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds =
    lens _detExportTaskIds (\s a -> s { _detExportTaskIds = a })

instance ToQuery DescribeExportTasks where
    toQuery = genericQuery def

newtype DescribeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks :: [ExportTask]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeExportTasksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExportTasks ::@ @[ExportTask]@
--
mkDescribeExportTasksResponse :: DescribeExportTasksResponse
mkDescribeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks = mempty
    }

-- | 
detrExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrExportTasks = lens _detrExportTasks (\s a -> s { _detrExportTasks = a })

instance FromXML DescribeExportTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeExportTasks where
    type Sv DescribeExportTasks = EC2
    type Rs DescribeExportTasks = DescribeExportTasksResponse

    request = post "DescribeExportTasks"
    response _ = xmlResponse