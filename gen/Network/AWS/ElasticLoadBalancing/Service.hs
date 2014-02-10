{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticLoadBalancing.Service where

import Network.AWS.Core
import Network.AWS.Generics.XML

-- | Currently supported version (@2012-06-01@) of the @Elastic Load Balancing@ service.
service :: Service
service = Service Global v4 "elasticloadbalancing" "2012-06-01" Nothing

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://elasticloadbalancing.amazonaws.com/doc/2012-06-01/"
    }

data ElasticLoadBalancingError
    = AccessPointNotFoundException
    | CertificateNotFoundException
    | DuplicateAccessPointNameException
    | DuplicateListenerException
    | DuplicatePolicyNameException
    | InvalidConfigurationRequestException
    | InvalidEndPointException
    | InvalidSchemeException
    | InvalidSecurityGroupException
    | InvalidSubnetException
    | ListenerNotFoundException
    | LoadBalancerAttributeNotFoundException
    | PolicyNotFoundException
    | PolicyTypeNotFoundException
    | SubnetNotFoundException
    | TooManyAccessPointsException
    | TooManyPoliciesException
      deriving (Eq, Show, Generic)

instance FromXML ElasticLoadBalancingError where
    fromXMLOptions = xmlOptions
