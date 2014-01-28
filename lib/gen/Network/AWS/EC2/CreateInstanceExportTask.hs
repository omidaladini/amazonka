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

data CreateInstanceExportTask = CreateInstanceExportTask
    { cietrDescription :: Maybe Text
    , cietrExportToS3Task :: Maybe ExportToS3TaskSpecification
    , cietrInstanceId :: !Text
    , cietrTargetEnvironment :: Maybe ExportEnvironment
    } deriving (Eq, Show, Generic)

instance ToQuery CreateInstanceExportTask

instance AWSRequest CreateInstanceExportTask where
    type Er CreateInstanceExportTask = EC2Error
    type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
    request = getQuery service "CreateInstanceExportTask"

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { cietrrsExportTask :: Maybe ExportTask
    } deriving (Eq, Show, Generic)

instance FromXML CreateInstanceExportTaskResponse where
    fromXMLOptions = xmlOptions
