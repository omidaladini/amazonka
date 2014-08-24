{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DeleteMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a metric filter associated with the specified log group. Delete a
-- metric filter The following is an example of a DeleteMetricFilter request
-- and response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteMetricFilter { "logGroupName":
-- "exampleLogGroupName", "filterName": "exampleMetricFilterName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.V2014_03_28.DeleteMetricFilter where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.V2014_03_28.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DeleteMetricFilter = DeleteMetricFilter
    { _dmftFilterName :: Text
      -- ^ The name of the metric filter.
    , _dmftLogGroupName :: Text
    } deriving (Show, Generic)

makeLenses ''DeleteMetricFilter

instance ToPath DeleteMetricFilter

instance ToQuery DeleteMetricFilter

instance ToHeaders DeleteMetricFilter

instance ToJSON DeleteMetricFilter

data DeleteMetricFilterResponse = DeleteMetricFilterResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteMetricFilterResponse

instance AWSRequest DeleteMetricFilter where
    type Sv DeleteMetricFilter = CloudWatchLogs
    type Rs DeleteMetricFilter = DeleteMetricFilterResponse

    request = get
    response _ = nullaryResponse DeleteMetricFilterResponse