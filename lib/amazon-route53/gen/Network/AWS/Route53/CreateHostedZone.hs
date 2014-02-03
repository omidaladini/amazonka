{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.CreateHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates a new hosted zone. To create a new hosted zone, send a
-- POST request to the 2012-12-12/hostedzone resource. The request body must
-- include an XML document with a CreateHostedZoneRequest element. The
-- response returns the CreateHostedZoneResponse element that contains
-- metadata about the hosted zone. Route 53 automatically creates a default
-- SOA record and four NS records for the zone. The NS records in the hosted
-- zone are the name servers you give your registrar to delegate your domain
-- to. For more information about SOA and NS records, see NS and SOA Records
-- that Route 53 Creates for a Hosted Zone in the Amazon Route 53 Developer
-- Guide. When you create a zone, its initial status is PENDING. This means
-- that it is not yet available on all DNS servers. The status of the zone
-- changes to INSYNC when the NS and SOA records are available on all Route 53
-- DNS servers.
module Network.AWS.Route53.CreateHostedZone where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createHostedZone :: Text
                 -> Text
                 -> CreateHostedZone
createHostedZone p1 p2 = CreateHostedZone
    { chzrCallerReference = p1
    , chzrName = p2
    , chzrHostedZoneConfig = Nothing
    }

data CreateHostedZone = CreateHostedZone
    { chzrCallerReference :: !Text
      -- ^ A unique string that identifies the request and that allows failed
      -- CreateHostedZone requests to be retried without the risk of executing the
      -- operation twice. You must use a unique CallerReference string every time
      -- you create a hosted zone. CallerReference can be any unique string; you
      -- might choose to use a string that identifies your project, such as
      -- DNSMigration_01. Valid characters are any Unicode code points that are
      -- legal in an XML 1.0 document. The UTF-8 encoding of the value must be less
      -- than 128 bytes.
    , chzrHostedZoneConfig :: Maybe HostedZoneConfig
      -- ^ A complex type that contains an optional comment about your hosted zone.
    , chzrName :: !Text
      -- ^ The name of the domain. This must be a fully-specified domain, for example,
      -- www.example.com. The trailing dot is optional; Route 53 assumes that the
      -- domain name is fully qualified. This means that Route 53 treats
      -- www.example.com (without a trailing dot) and www.example.com. (with a
      -- trailing dot) as identical. This is the name you have registered with your
      -- DNS registrar. You should ask your registrar to change the authoritative
      -- name servers for your domain to the set of NameServers elements returned in
      -- DelegationSet.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateHostedZone

instance ToPath CreateHostedZone where
    toPath = const "/2012-12-12/hostedzone"

instance ToQuery CreateHostedZone where
    toQuery = const mempty

instance ToXML CreateHostedZone where
    toXMLOptions = xmlOptions

instance AWSRequest CreateHostedZone where
    type Er CreateHostedZone = Route53Error
    type Rs CreateHostedZone = CreateHostedZoneResponse
    request = postRestXML service

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { chzrrsChangeInfo :: ChangeInfo
      -- ^ A complex type that contains information about the request to create a
      -- hosted zone. This includes an ID that you use when you call the GetChange
      -- action to get the current status of the change request.
    , chzrrsDelegationSet :: DelegationSet
      -- ^ A complex type that contains name server information.
    , chzrrsHostedZone :: HostedZone
      -- ^ A complex type that contains identifying information about the hosted zone.
    , chzrrsLocation :: !Text
      -- ^ The unique URL representing the new hosted zone.
    } deriving (Eq, Show, Generic)

instance FromXML CreateHostedZoneResponse where
    fromXMLOptions = xmlOptions
