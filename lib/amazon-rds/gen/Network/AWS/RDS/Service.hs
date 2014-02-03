{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.RDS.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2013-09-09@) of the @Amazon Relational Database Service@ service.
service :: Service
service = Service Global v4 "rds" "2013-09-09"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://rds.amazonaws.com/doc/2013-09-09/"
    }

data RDSError
    = AuthorizationAlreadyExistsFault
    | AuthorizationNotFoundFault
    | AuthorizationQuotaExceededFault
    | DBInstanceAlreadyExistsFault
    | DBInstanceNotFoundFault
    | DBParameterGroupAlreadyExistsFault
    | DBParameterGroupNotFoundFault
    | DBParameterGroupQuotaExceededFault
    | DBSecurityGroupAlreadyExistsFault
    | DBSecurityGroupNotFoundFault
    | DBSecurityGroupNotSupportedFault
    | DBSecurityGroupQuotaExceededFault
    | DBSnapshotAlreadyExistsFault
    | DBSnapshotNotFoundFault
    | DBSubnetGroupAlreadyExistsFault
    | DBSubnetGroupDoesNotCoverEnoughAZs
    | DBSubnetGroupNotAllowedFault
    | DBSubnetGroupNotFoundFault
    | DBSubnetGroupQuotaExceededFault
    | DBSubnetQuotaExceededFault
    | DBUpgradeDependencyFailureFault
    | EventSubscriptionQuotaExceededFault
    | InstanceQuotaExceededFault
    | InsufficientDBInstanceCapacityFault
    | InvalidDBInstanceStateFault
    | InvalidDBParameterGroupStateFault
    | InvalidDBSecurityGroupStateFault
    | InvalidDBSnapshotStateFault
    | InvalidDBSubnetGroupFault
    | InvalidDBSubnetGroupStateFault
    | InvalidDBSubnetStateFault
    | InvalidEventSubscriptionStateFault
    | InvalidOptionGroupStateFault
    | InvalidRestoreFault
    | InvalidSubnet
    | InvalidVPCNetworkStateFault
    | OptionGroupAlreadyExistsFault
    | OptionGroupNotFoundFault
    | OptionGroupQuotaExceededFault
    | PointInTimeRestoreNotEnabledFault
    | ProvisionedIopsNotAvailableInAZFault
    | ReservedDBInstanceAlreadyExistsFault
    | ReservedDBInstanceNotFoundFault
    | ReservedDBInstanceQuotaExceededFault
    | ReservedDBInstancesOfferingNotFoundFault
    | SNSInvalidTopicFault
    | SNSNoAuthorizationFault
    | SNSTopicArnNotFoundFault
    | SnapshotQuotaExceededFault
    | SourceNotFoundFault
    | StorageQuotaExceededFault
    | SubnetAlreadyInUse
    | SubscriptionAlreadyExistFault
    | SubscriptionCategoryNotFoundFault
    | SubscriptionNotFoundFault
      deriving (Eq, Show, Generic)

instance FromXML RDSError where
    fromXMLOptions = xmlOptions
