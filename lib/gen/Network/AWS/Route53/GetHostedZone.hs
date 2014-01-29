{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve the delegation set for a hosted zone, send a GET request to the
-- 2012-12-12/hostedzone/hosted zone ID resource. The delegation set is the
-- four Route 53 name servers that were assigned to the hosted zone when you
-- created it.
module Network.AWS.Route53.GetHostedZone where

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
getHostedZone :: Text -- ^ Id
              -> AWS (Either Route53Error GetHostedZoneResponse)
getHostedZone p1 = undefined $ GetHostedZone
    { ghzrId = p1
    }

data GetHostedZone = GetHostedZone
    { ghzrId :: !Text
      -- ^ The ID of the hosted zone for which you want to get a list of the name
      -- servers in the delegation set.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetHostedZone

instance ToPath GetHostedZone where
    toPath GetHostedZone{..} = Text.concat
        [ "/2012-12-12/hostedzone/"
        , toText ghzrId
        ]

instance ToQuery GetHostedZone where
    toQuery = const mempty

instance ToXML GetHostedZone where
    toXMLOptions = xmlOptions

instance AWSRequest GetHostedZone where
    type Er GetHostedZone = Route53Error
    type Rs GetHostedZone = GetHostedZoneResponse
    request = getRestXML service

data GetHostedZoneResponse = GetHostedZoneResponse
    { ghzrrsDelegationSet :: DelegationSet
      -- ^ A complex type that contains information about the name servers for the
      -- specified hosted zone.
    , ghzrrsHostedZone :: HostedZone
      -- ^ A complex type that contains the information about the specified hosted
      -- zone.
    } deriving (Eq, Show, Generic)

instance FromXML GetHostedZoneResponse where
    fromXMLOptions = xmlOptions
