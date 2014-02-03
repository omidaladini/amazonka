{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
module Network.AWS.CloudWatch.DeleteAlarms where

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

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteAlarms :: [Text]
             -> DeleteAlarms
deleteAlarms p1 = DeleteAlarms
    { dajAlarmNames = p1
    }

data DeleteAlarms = DeleteAlarms
    { dajAlarmNames :: [Text]
      -- ^ A list of alarms to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteAlarms

instance AWSRequest DeleteAlarms where
    type Er DeleteAlarms = CloudWatchError
    type Rs DeleteAlarms = DeleteAlarmsResponse
    request = getQuery service "DeleteAlarms"

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAlarmsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteAlarmsResponse
