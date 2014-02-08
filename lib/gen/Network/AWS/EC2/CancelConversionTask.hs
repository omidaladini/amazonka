{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CancelConversionTask
module Network.AWS.EC2.CancelConversionTask where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
cancelConversionTask :: Text
                     -> CancelConversionTask
cancelConversionTask p1 = CancelConversionTask
    { ccrConversionTaskId = p1
    , ccrDryRun = Nothing
    , ccrReasonMessage = Nothing
    }

data CancelConversionTask = CancelConversionTask
    { ccrConversionTaskId :: !Text
    , ccrDryRun :: Maybe Bool
    , ccrReasonMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery CancelConversionTask

instance AWSRequest CancelConversionTask where
    type Er CancelConversionTask = EC2Error
    type Rs CancelConversionTask = CancelConversionTaskResponse
    request = getQuery service "CancelConversionTask"

data CancelConversionTaskResponse = CancelConversionTaskResponse
    deriving (Eq, Show, Generic)

instance FromXML CancelConversionTaskResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CancelConversionTaskResponse"
