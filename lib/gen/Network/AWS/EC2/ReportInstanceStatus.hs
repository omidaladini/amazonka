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

-- | Convenience method utilising default fields where applicable.
reportInstanceStatus :: AWS (Either EC2Error ReportInstanceStatusResponse)
reportInstanceStatus = undefined $ ReportInstanceStatus
    { risrDescription = Nothing
    , risrDryRun = Nothing
    , risrEndTime = Nothing
    , risrInstances = []
    , risrReasonCodes = []
    , risrStartTime = Nothing
    , risrStatus = Nothing
    }

data ReportInstanceStatus = ReportInstanceStatus
    { risrDescription :: Maybe Text
    , risrDryRun :: Maybe Bool
    , risrEndTime :: Maybe UTCTime
    , risrInstances :: [Text]
    , risrReasonCodes :: [ReportInstanceReasonCodes]
    , risrStartTime :: Maybe UTCTime
    , risrStatus :: Maybe ReportStatusType
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
