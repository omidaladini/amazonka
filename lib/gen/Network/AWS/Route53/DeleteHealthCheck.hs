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

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.Route53.Service
import Network.AWS.Route53.Types

-- | Convenience method utilising default fields where applicable.
deleteHealthCheck :: Text -- ^ HealthCheckId
                  -> AWS (Either Route53Error DeleteHealthCheckResponse)
deleteHealthCheck p1 = undefined $ DeleteHealthCheck
    { dhcrHealthCheckId = p1
    }

data DeleteHealthCheck = DeleteHealthCheck
    { dhcrHealthCheckId :: !Text
      -- ^ The ID of the health check to delete.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteHealthCheck

instance ToPath DeleteHealthCheck where
    toPath DeleteHealthCheck{..} = Text.concat
        [ "/2012-12-12/healthcheck/"
        , toText dhcrHealthCheckId
        ]

instance ToQuery DeleteHealthCheck where
    toQuery = const mempty

instance ToXML DeleteHealthCheck where
    toXMLOptions = xmlOptions

instance AWSRequest DeleteHealthCheck where
    type Er DeleteHealthCheck = Route53Error
    type Rs DeleteHealthCheck = DeleteHealthCheckResponse
    request = deleteRestXML service

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteHealthCheckResponse where
    fromXMLOptions = xmlOptions
