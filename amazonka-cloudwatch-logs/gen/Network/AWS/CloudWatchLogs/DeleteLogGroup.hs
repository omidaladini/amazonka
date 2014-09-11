{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the log group with the specified name and permanently deletes all
-- the archived log events associated with it. Delete a Log Group The
-- following is an example of a DeleteLogGroup request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteLogGroup { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.DeleteLogGroup
    (
    -- * Request
      DeleteLogGroup
    -- ** Request constructor
    , mkDeleteLogGroup
    -- ** Request lenses
    , dlgLogGroupName

    -- * Response
    , DeleteLogGroupResponse
    -- ** Response constructor
    , mkDeleteLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteLogGroup = DeleteLogGroup
    { _dlgLogGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLogGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
mkDeleteLogGroup :: Text -- ^ 'dlgLogGroupName'
                 -> DeleteLogGroup
mkDeleteLogGroup p1 = DeleteLogGroup
    { _dlgLogGroupName = p1
    }

dlgLogGroupName :: Lens' DeleteLogGroup Text
dlgLogGroupName = lens _dlgLogGroupName (\s a -> s { _dlgLogGroupName = a })

instance ToPath DeleteLogGroup

instance ToQuery DeleteLogGroup

instance ToHeaders DeleteLogGroup

instance ToJSON DeleteLogGroup

data DeleteLogGroupResponse = DeleteLogGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLogGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLogGroupResponse :: DeleteLogGroupResponse
mkDeleteLogGroupResponse = DeleteLogGroupResponse

instance AWSRequest DeleteLogGroup where
    type Sv DeleteLogGroup = CloudWatchLogs
    type Rs DeleteLogGroup = DeleteLogGroupResponse

    request = get
    response _ = nullaryResponse DeleteLogGroupResponse