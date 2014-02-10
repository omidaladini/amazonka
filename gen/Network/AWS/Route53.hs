-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53
    (
    -- * Operations
    -- ** GetChange
      module Network.AWS.Route53.GetChange
    -- ** ChangeResourceRecordSets
    , module Network.AWS.Route53.ChangeResourceRecordSets
    -- ** DeleteHealthCheck
    , module Network.AWS.Route53.DeleteHealthCheck
    -- ** CreateHostedZone
    , module Network.AWS.Route53.CreateHostedZone
    -- ** CreateHealthCheck
    , module Network.AWS.Route53.CreateHealthCheck
    -- ** ListHostedZones
    , module Network.AWS.Route53.ListHostedZones
    -- ** GetHostedZone
    , module Network.AWS.Route53.GetHostedZone
    -- ** GetHealthCheck
    , module Network.AWS.Route53.GetHealthCheck
    -- ** ListResourceRecordSets
    , module Network.AWS.Route53.ListResourceRecordSets
    -- ** ListHealthChecks
    , module Network.AWS.Route53.ListHealthChecks
    -- ** DeleteHostedZone
    , module Network.AWS.Route53.DeleteHostedZone

    -- * Types
    -- ** ResourceRecordSet
    , ResourceRecordSet (..)
    -- ** ResourceRecord
    , ResourceRecord (..)
    -- ** HostedZoneConfig
    , HostedZoneConfig (..)
    -- ** HostedZone
    , HostedZone (..)
    -- ** HealthCheckConfig
    , HealthCheckConfig (..)
    -- ** HealthCheck
    , HealthCheck (..)
    -- ** DelegationSet
    , DelegationSet (..)
    -- ** ChangeInfo
    , ChangeInfo (..)
    -- ** ChangeBatch
    , ChangeBatch (..)
    -- ** Change
    , Change (..)
    -- ** AliasTarget
    , AliasTarget (..)
    -- ** ResourceRecordSetRegion
    , ResourceRecordSetRegion (..)
    -- ** ResourceRecordSetFailover
    , ResourceRecordSetFailover (..)
    -- ** RRType
    , RRType (..)
    -- ** HealthCheckType
    , HealthCheckType (..)
    -- ** ChangeStatus
    , ChangeStatus (..)
    -- ** ChangeAction
    , ChangeAction (..)

    -- * Errors
    , Route53Error (..)
    ) where

import Network.AWS.Route53.Service
import Network.AWS.Route53.Types

import Network.AWS.Route53.GetChange
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.DeleteHostedZone
