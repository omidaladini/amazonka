{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Service where

import Network.AWS.Core
import Network.AWS.Generics.XML

-- | Currently supported version (@2012-12-12@) of the @Amazon Route 53@ service.
service :: Service
service = Service Global v3 "route53" "2012-12-12"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "https://route53.amazonaws.com/doc/2012-12-12/"
    }

data Route53Error
    = DelegationSetNotAvailable
    | HealthCheckAlreadyExists
    | HealthCheckInUse
    | HostedZoneAlreadyExists
    | HostedZoneNotEmpty
    | InvalidChangeBatch
    | InvalidDomainName
    | InvalidInput
    | NoSuchChange
    | NoSuchHealthCheck
    | NoSuchHostedZone
    | PriorRequestNotComplete
    | TooManyHealthChecks
    | TooManyHostedZones
      deriving (Eq, Show, Generic)

instance FromXML Route53Error where
    fromXMLOptions = xmlOptions
