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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ReportInstanceStatus = ReportInstanceStatus
    { risrDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , risrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , risrEndTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , risrInstances :: [Text]
      -- ^ FIXME: Missing documentation
    , risrReasonCodes :: [ReportInstanceReasonCodes]
      -- ^ FIXME: Missing documentation
    , risrStartTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , risrStatus :: Maybe ReportStatusType
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ReportInstanceStatus

instance AWSRequest ReportInstanceStatus where
    type Er ReportInstanceStatus = EC2Error
    type Rs ReportInstanceStatus = ReportInstanceStatusResponse
    request = v2Query service GET "ReportInstanceStatus"

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    deriving (Eq, Show, Generic)

instance FromXML ReportInstanceStatusResponse where
    fromXMLOptions = xmlOptions
