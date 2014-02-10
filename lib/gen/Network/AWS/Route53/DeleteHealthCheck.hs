{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action deletes a health check. To delete a health check, send a DELETE
-- request to the 2012-12-12/healthcheck/health check ID resource. You can
-- delete a health check only if there are no resource record sets associated
-- with this health check. If resource record sets are associated with this
-- health check, you must disassociate them before you can delete your health
-- check. If you try to delete a health check that is associated with resource
-- record sets, Route 53 will deny your request with a HealthCheckInUse error.
-- For information about disassociating the records from your health check,
-- see ChangeResourceRecordSets.
module Network.AWS.Route53.DeleteHealthCheck where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.Route53.Service
import           Network.AWS.Route53.Types

data DeleteHealthCheck = DeleteHealthCheck
    { dhcHealthCheckId :: !Text
      -- ^ The ID of the health check to delete.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteHealthCheck

instance ToPath DeleteHealthCheck where
    toPath DeleteHealthCheck{..} = Text.concat
        [ "/2012-12-12/healthcheck/"
        , toText dhcHealthCheckId
        ]

instance ToQuery DeleteHealthCheck where
    toQuery = const mempty

instance AWSRequest DeleteHealthCheck where
    type Er DeleteHealthCheck = Route53Error
    type Rs DeleteHealthCheck = DeleteHealthCheckResponse
    request  = deleteRestXML service
    response = responseXML

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteHealthCheckResponse where
    fromXMLOptions = xmlOptions