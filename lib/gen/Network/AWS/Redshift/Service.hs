{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Redshift.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2012-12-01@) of the @Amazon Redshift@ service.
service :: Service
service = Service Global v4 "redshift" "2012-12-01"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://redshift.amazonaws.com/doc/2012-12-01/"
    }

data RedshiftError
    = AccessToSnapshotDeniedFault
    | AuthorizationAlreadyExistsFault
    | AuthorizationNotFoundFault
    | AuthorizationQuotaExceededFault
    | BucketNotFoundFault
    | ClusterAlreadyExistsFault
    | ClusterNotFoundFault
    | ClusterParameterGroupAlreadyExistsFault
    | ClusterParameterGroupNotFoundFault
    | ClusterParameterGroupQuotaExceededFault
    | ClusterQuotaExceededFault
    | ClusterSecurityGroupAlreadyExistsFault
    | ClusterSecurityGroupNotFoundFault
    | ClusterSecurityGroupQuotaExceededFault
    | ClusterSnapshotAlreadyExistsFault
    | ClusterSnapshotNotFoundFault
    | ClusterSnapshotQuotaExceededFault
    | ClusterSubnetGroupAlreadyExistsFault
    | ClusterSubnetGroupNotFoundFault
    | ClusterSubnetGroupQuotaExceededFault
    | ClusterSubnetQuotaExceededFault
    | CopyToRegionDisabledFault
    | EventSubscriptionQuotaExceededFault
    | HsmClientCertificateAlreadyExistsFault
    | HsmClientCertificateNotFoundFault
    | HsmClientCertificateQuotaExceededFault
    | HsmConfigurationAlreadyExistsFault
    | HsmConfigurationNotFoundFault
    | HsmConfigurationQuotaExceededFault
    | IncompatibleOrderableOptions
    | InsufficientClusterCapacityFault
    | InsufficientS3BucketPolicyFault
    | InvalidClusterParameterGroupStateFault
    | InvalidClusterSecurityGroupStateFault
    | InvalidClusterSnapshotStateFault
    | InvalidClusterStateFault
    | InvalidClusterSubnetGroupStateFault
    | InvalidClusterSubnetStateFault
    | InvalidElasticIpFault
    | InvalidHsmClientCertificateStateFault
    | InvalidHsmConfigurationStateFault
    | InvalidRestoreFault
    | InvalidS3BucketNameFault
    | InvalidS3KeyPrefixFault
    | InvalidSubnet
    | InvalidVPCNetworkStateFault
    | NumberOfNodesPerClusterLimitExceededFault
    | NumberOfNodesQuotaExceededFault
    | ReservedNodeAlreadyExistsFault
    | ReservedNodeNotFoundFault
    | ReservedNodeOfferingNotFoundFault
    | ReservedNodeQuotaExceededFault
    | ResizeNotFoundFault
    | SNSInvalidTopicFault
    | SNSNoAuthorizationFault
    | SNSTopicArnNotFoundFault
    | SnapshotCopyAlreadyDisabledFault
    | SnapshotCopyAlreadyEnabledFault
    | SnapshotCopyDisabledFault
    | SourceNotFoundFault
    | SubnetAlreadyInUse
    | SubscriptionAlreadyExistFault
    | SubscriptionCategoryNotFoundFault
    | SubscriptionEventIdNotFoundFault
    | SubscriptionNotFoundFault
    | SubscriptionSeverityNotFoundFault
    | UnauthorizedOperation
    | UnknownSnapshotCopyRegionFault
    | UnsupportedOptionFault
      deriving (Eq, Show, Generic)

instance FromXML RedshiftError where
    fromXMLOptions = xmlOptions
