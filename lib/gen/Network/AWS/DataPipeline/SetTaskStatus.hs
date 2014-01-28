{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Notifies AWS Data Pipeline that a task is completed and provides
-- information about the final status. The task runner calls this action
-- regardless of whether the task was sucessful. The task runner does not need
-- to call SetTaskStatus for tasks that are canceled by the web service during
-- a call to ReportTaskProgress. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.SetTaskStatus
-- Content-Length: 847 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "taskStatus": "FINISHED"} x-amzn-RequestId:
-- 8c8deb53-0788-11e2-af9c-6bc7a6be6qr8 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
module Network.AWS.DataPipeline.SetTaskStatus where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DataPipeline.Service
import Network.AWS.DataPipeline.Types

data SetTaskStatus = SetTaskStatus
    { stsierrorId :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies an id value that
      -- represents the error. This value is set on the physical attempt object. It
      -- is used to display error information to the user. It should not start with
      -- string "Service_" which is reserved by the system.
    , stsierrorMessage :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies a text
      -- description of the error. This value is set on the physical attempt object.
      -- It is used to display error information to the user. The web service does
      -- not parse this value.
    , stsierrorStackTrace :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies the stack trace
      -- associated with the error. This value is set on the physical attempt
      -- object. It is used to display error information to the user. The web
      -- service does not parse this value.
    , stsitaskId :: !Text
      -- ^ Identifies the task assigned to the task runner. This value is set in the
      -- TaskObject that is returned by the PollForTask action.
    , stsitaskStatus :: !TaskStatus
      -- ^ If FINISHED, the task successfully completed. If FAILED the task ended
      -- unsuccessfully. The FALSE value is used by preconditions.
    } deriving (Eq, Show, Generic)

instance ToJSON SetTaskStatus

instance AWSRequest SetTaskStatus where
    type Er SetTaskStatus = DataPipelineError
    type Rs SetTaskStatus = SetTaskStatusResponse
    request  = getJSON service
    response = responseJSON

data SetTaskStatusResponse = SetTaskStatusResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetTaskStatusResponse
