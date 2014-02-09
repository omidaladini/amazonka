{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | CancelBundleTask operation cancels a pending or in-progress bundling task.
-- This is an asynchronous call and it make take a while for the task to be
-- canceled. If a task is canceled while it is storing items, there may be
-- parts of the incomplete AMI stored in S3. It is up to the caller to clean
-- up these parts from S3.
module Network.AWS.EC2.CancelBundleTask where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
cancelBundleTask :: Text
                 -- ^ The ID of the bundle task to cancel.
                 -> CancelBundleTask
cancelBundleTask p1 = CancelBundleTask
    { cbtBundleId = p1
    , cbtDryRun = Nothing
    }

data CancelBundleTask = CancelBundleTask
    { cbtBundleId :: !Text
      -- ^ The ID of the bundle task to cancel.
    , cbtDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery CancelBundleTask

instance AWSRequest CancelBundleTask where
    type Er CancelBundleTask = EC2Error
    type Rs CancelBundleTask = CancelBundleTaskResponse
    request = getQuery service "CancelBundleTask"

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { cbtrBundleTask :: Maybe BundleTask
      -- ^ The canceled bundle task.
    } deriving (Eq, Show, Generic)

instance FromXML CancelBundleTaskResponse where
    fromXMLOptions = xmlOptions
