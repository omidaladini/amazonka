{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action deletes a hosted zone. To delete a hosted zone, send a DELETE
-- request to the 2012-12-12/hostedzone/hosted zone ID resource. For more
-- information about deleting a hosted zone, see Deleting a Hosted Zone in the
-- Amazon Route 53 Developer Guide. You can delete a hosted zone only if there
-- are no resource record sets other than the default SOA record and NS
-- resource record sets. If your hosted zone contains other resource record
-- sets, you must delete them before you can delete your hosted zone. If you
-- try to delete a hosted zone that contains other resource record sets, Route
-- 53 will deny your request with a HostedZoneNotEmpty error. For information
-- about deleting records from your hosted zone, see ChangeResourceRecordSets.
module Network.AWS.Route53.DeleteHostedZone where

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
deleteHostedZone :: Text
                 -> AWS (Either Route53Error DeleteHostedZoneResponse)
deleteHostedZone p1 = undefined $ DeleteHostedZone
    { dhzrId = p1
    }

data DeleteHostedZone = DeleteHostedZone
    { dhzrId :: !Text
      -- ^ The ID of the request. Include this ID in a call to GetChange to track when
      -- the change has propagated to all Route 53 DNS servers.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteHostedZone

instance ToPath DeleteHostedZone where
    toPath DeleteHostedZone{..} = Text.concat
        [ "/2012-12-12/hostedzone/"
        , toText dhzrId
        ]

instance ToQuery DeleteHostedZone where
    toQuery = const mempty

instance ToXML DeleteHostedZone where
    toXMLOptions = xmlOptions

instance AWSRequest DeleteHostedZone where
    type Er DeleteHostedZone = Route53Error
    type Rs DeleteHostedZone = DeleteHostedZoneResponse
    request = deleteRestXML service

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { dhzrrsChangeInfo :: ChangeInfo
      -- ^ A complex type that contains the ID, the status, and the date and time of
      -- your delete request.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteHostedZoneResponse where
    fromXMLOptions = xmlOptions
