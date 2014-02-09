{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ReportInstanceStatus
module Network.AWS.EC2.ReportInstanceStatus where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ReportInstanceStatus = ReportInstanceStatus
    { risDescription :: Maybe Text
    , risDryRun :: Maybe Bool
    , risEndTime :: Maybe UTCTime
    , risInstances :: [Text]
    , risReasonCodes :: [ReportInstanceReasonCodes]
    , risStartTime :: Maybe UTCTime
    , risStatus :: Maybe ReportStatusType
    } deriving (Eq, Show, Generic)

instance ToQuery ReportInstanceStatus

instance AWSRequest ReportInstanceStatus where
    type Er ReportInstanceStatus = EC2Error
    type Rs ReportInstanceStatus = ReportInstanceStatusResponse
    request = getQuery service "ReportInstanceStatus"

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    deriving (Eq, Show, Generic)

instance FromXML ReportInstanceStatusResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReportInstanceStatusResponse"
