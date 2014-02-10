{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelExportTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CancelExportTask
module Network.AWS.EC2.CancelExportTask where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CancelExportTask = CancelExportTask
    { cetExportTaskId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CancelExportTask

instance AWSRequest CancelExportTask where
    type Er CancelExportTask = EC2Error
    type Rs CancelExportTask = CancelExportTaskResponse
    request  = postQuery service "CancelExportTask"
    response = responseXML

data CancelExportTaskResponse = CancelExportTaskResponse
    deriving (Eq, Show, Generic)

instance FromXML CancelExportTaskResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CancelExportTaskResponse"
