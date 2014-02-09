{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateInstanceExportTask
module Network.AWS.EC2.CreateInstanceExportTask where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createInstanceExportTask :: Text
                         -> CreateInstanceExportTask
createInstanceExportTask p1 = CreateInstanceExportTask
    { cietInstanceId = p1
    , cietDescription = Nothing
    , cietExportToS3Task = Nothing
    , cietTargetEnvironment = Nothing
    }

data CreateInstanceExportTask = CreateInstanceExportTask
    { cietDescription :: Maybe Text
    , cietExportToS3Task :: Maybe ExportToS3TaskSpecification
    , cietInstanceId :: !Text
    , cietTargetEnvironment :: Maybe ExportEnvironment
    } deriving (Eq, Show, Generic)

instance ToQuery CreateInstanceExportTask

instance AWSRequest CreateInstanceExportTask where
    type Er CreateInstanceExportTask = EC2Error
    type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
    request = getQuery service "CreateInstanceExportTask"

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { cietrExportTask :: Maybe ExportTask
    } deriving (Eq, Show, Generic)

instance FromXML CreateInstanceExportTaskResponse where
    fromXMLOptions = xmlOptions
