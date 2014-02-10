{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Types where

import Network.AWS.Core
import Network.AWS.Route53.Service

-- | Information about the resource record set to create or delete.
data ResourceRecordSet = ResourceRecordSet
    { rrsAliasTarget :: Maybe AliasTarget
      -- ^ Alias resource record sets only: Information about the AWS resource to
      -- which you are redirecting traffic.
    , rrsFailover :: Maybe ResourceRecordSetFailover
      -- ^ Failover resource record sets only: Among resource record sets that have
      -- the same combination of DNS name and type, a value that indicates whether
      -- the current resource record set is a primary or secondary resource record
      -- set. A failover set may contain at most one resource record set marked as
      -- primary and one resource record set marked as secondary. A resource record
      -- set marked as primary will be returned if any of the following are true:
      -- (1) an associated health check is passing, (2) if the resource record set
      -- is an alias with the evaluate target health and at least one target
      -- resource record set is healthy, (3) both the primary and secondary resource
      -- record set are failing health checks or (4) there is no secondary resource
      -- record set. A secondary resource record set will be returned if: (1) the
      -- primary is failing a health check and either the secondary is passing a
      -- health check or has no associated health check, or (2) there is no primary
      -- resource record set. Valid values: PRIMARY | SECONDARY.
    , rrsHealthCheckId :: Maybe Text
      -- ^ Health Check resource record sets only, not required for alias resource
      -- record sets: An identifier that is used to identify health check associated
      -- with the resource record set.
    , rrsName :: !Text
      -- ^ The domain name of the current resource record set.
    , rrsRegion :: Maybe ResourceRecordSetRegion
      -- ^ Regional resource record sets only: Among resource record sets that have
      -- the same combination of DNS name and type, a value that specifies the AWS
      -- region for the current resource record set.
    , rrsResourceRecords :: [ResourceRecord]
      -- ^ A complex type that contains the resource records for the current resource
      -- record set.
    , rrsSetIdentifier :: Maybe Text
      -- ^ Weighted, Regional, and Failover resource record sets only: An identifier
      -- that differentiates among multiple resource record sets that have the same
      -- combination of DNS name and type.
    , rrsTTL :: Maybe Integer
      -- ^ The cache time to live for the current resource record set.
    , rrsType :: !RRType
      -- ^ The type of the current resource record set.
    , rrsWeight :: Maybe Integer
      -- ^ Weighted resource record sets only: Among resource record sets that have
      -- the same combination of DNS name and type, a value that determines what
      -- portion of traffic for the current resource record set is routed to the
      -- associated location.
    } deriving (Eq, Show, Generic)

instance ToQuery ResourceRecordSet

instance FromXML ResourceRecordSet where
    fromXMLOptions = xmlOptions

instance ToXML ResourceRecordSet where
    toXMLOptions = xmlOptions

-- | A complex type that contains the value of the Value element for the current
-- resource record set.
newtype ResourceRecord = ResourceRecord
    { rrValue :: Text
      -- ^ The value of the Value element for the current resource record set.
    } deriving (Eq, Show, Generic)

instance ToQuery ResourceRecord

instance FromXML ResourceRecord where
    fromXMLOptions = xmlOptions

instance ToXML ResourceRecord where
    toXMLOptions = xmlOptions

-- | A complex type that contains an optional comment about your hosted zone.
newtype HostedZoneConfig = HostedZoneConfig
    { hzcComment :: Maybe Text
      -- ^ An optional comment about your hosted zone. If you don't want to specify a
      -- comment, you can omit the HostedZoneConfig and Comment elements from the
      -- XML document.
    } deriving (Eq, Show, Generic)

instance ToQuery HostedZoneConfig

instance FromXML HostedZoneConfig where
    fromXMLOptions = xmlOptions

instance ToXML HostedZoneConfig where
    toXMLOptions = xmlOptions

-- | A complex type that contains identifying information about the hosted zone.
data HostedZone = HostedZone
    { hzCallerReference :: !Text
      -- ^ A unique string that identifies the request to create the hosted zone.
    , hzConfig :: Maybe HostedZoneConfig
      -- ^ A complex type that contains the Comment element.
    , hzId :: !Text
      -- ^ The ID of the specified hosted zone.
    , hzName :: !Text
      -- ^ The name of the domain. This must be a fully-specified domain, for example,
      -- www.example.com. The trailing dot is optional; Route 53 assumes that the
      -- domain name is fully qualified. This means that Route 53 treats
      -- www.example.com (without a trailing dot) and www.example.com. (with a
      -- trailing dot) as identical. This is the name you have registered with your
      -- DNS registrar. You should ask your registrar to change the authoritative
      -- name servers for your domain to the set of NameServers elements returned in
      -- DelegationSet.
    , hzResourceRecordSetCount :: Maybe Integer
      -- ^ Total number of resource record sets in the hosted zone.
    } deriving (Eq, Show, Generic)

instance ToQuery HostedZone

instance FromXML HostedZone where
    fromXMLOptions = xmlOptions

instance ToXML HostedZone where
    toXMLOptions = xmlOptions

-- | A complex type that contains health check configuration.
data HealthCheckConfig = HealthCheckConfig
    { hccFullyQualifiedDomainName :: Maybe Text
      -- ^ Fully qualified domain name of the instance to be health checked.
    , hccIPAddress :: !Text
      -- ^ IP Address of the instance being checked.
    , hccPort :: Maybe Int
      -- ^ Port on which connection will be opened to the instance to health check.
      -- For HTTP this defaults to 80 if the port is not specified.
    , hccResourcePath :: Maybe Text
      -- ^ Path to ping on the instance to check the health. Required only for HTTP
      -- health checks, HTTP request is issued to the instance on the given port and
      -- path.
    , hccType :: !HealthCheckType
      -- ^ The type of health check to be performed. Currently supported protocols are
      -- TCP and HTTP.
    } deriving (Eq, Show, Generic)

instance ToQuery HealthCheckConfig

instance FromXML HealthCheckConfig where
    fromXMLOptions = xmlOptions

instance ToXML HealthCheckConfig where
    toXMLOptions = xmlOptions

-- | A complex type that contains identifying information about the health
-- check.
data HealthCheck = HealthCheck
    { hcCallerReference :: !Text
      -- ^ A unique string that identifies the request to create the health check.
    , hcHealthCheckConfig :: HealthCheckConfig
      -- ^ A complex type that contains the health check configuration.
    , hcId :: !Text
      -- ^ The ID of the specified health check.
    } deriving (Eq, Show, Generic)

instance ToQuery HealthCheck

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions

instance ToXML HealthCheck where
    toXMLOptions = xmlOptions

-- | A complex type that contains name server information.
newtype DelegationSet = DelegationSet
    { dsNameServers :: [Text]
      -- ^ A complex type that contains the authoritative name servers for the hosted
      -- zone. Use the method provided by your domain registrar to add an NS record
      -- to your domain for each NameServer that is assigned to your hosted zone.
    } deriving (Eq, Show, Generic)

instance ToQuery DelegationSet

instance FromXML DelegationSet where
    fromXMLOptions = xmlOptions

instance ToXML DelegationSet where
    toXMLOptions = xmlOptions

-- | A complex type that contains information about the specified change batch,
-- including the change batch ID, the status of the change, and the date and
-- time of the request.
data ChangeInfo = ChangeInfo
    { ciComment :: Maybe Text
      -- ^ A complex type that describes change information about changes made to your
      -- hosted zone. This element contains an ID that you use when performing a
      -- GetChange action to get detailed information about the change.
    , ciId :: !Text
      -- ^ The ID of the request. Use this ID to track when the change has completed
      -- across all Amazon Route 53 DNS servers.
    , ciStatus :: !ChangeStatus
      -- ^ The current state of the request. PENDING indicates that this request has
      -- not yet been applied to all Amazon Route 53 DNS servers. Valid Values:
      -- PENDING | INSYNC.
    , ciSubmittedAt :: !UTCTime
      -- ^ The date and time the change was submitted, in the format
      -- YYYY-MM-DDThh:mm:ssZ, as specified in the ISO 8601 standard (for example,
      -- 2009-11-19T19:37:58Z). The Z after the time indicates that the time is
      -- listed in Coordinated Universal Time (UTC), which is synonymous with
      -- Greenwich Mean Time in this context.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeInfo

instance FromXML ChangeInfo where
    fromXMLOptions = xmlOptions

instance ToXML ChangeInfo where
    toXMLOptions = xmlOptions

-- | A complex type that contains an optional comment and the Changes element.
data ChangeBatch = ChangeBatch
    { cbChanges :: [Change]
      -- ^ A complex type that contains one Change element for each resource record
      -- set that you want to create or delete.
    , cbComment :: Maybe Text
      -- ^ Optional: Any comments you want to include about a change batch request.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeBatch

instance FromXML ChangeBatch where
    fromXMLOptions = xmlOptions

instance ToXML ChangeBatch where
    toXMLOptions = xmlOptions

-- | A complex type that contains the information for each change in a change
-- batch request.
data Change = Change
    { cAction :: !ChangeAction
      -- ^ The action to perform. Valid values: CREATE | DELETE.
    , cResourceRecordSet :: ResourceRecordSet
      -- ^ Information about the resource record set to create or delete.
    } deriving (Eq, Show, Generic)

instance ToQuery Change

instance FromXML Change where
    fromXMLOptions = xmlOptions

instance ToXML Change where
    toXMLOptions = xmlOptions

-- | Alias resource record sets only: Information about the AWS resource to
-- which you are redirecting traffic.
data AliasTarget = AliasTarget
    { atDNSName :: !Text
      -- ^ Alias resource record sets only: The external DNS name associated with the
      -- AWS Resource. For more information and an example, see Creating Alias
      -- Resource Record Sets in the Amazon Route 53 Developer Guide.
    , atEvaluateTargetHealth :: !Bool
      -- ^ Alias resource record sets only: A boolean value that indicates whether
      -- this Resource Record Set should respect the health status of any health
      -- checks associated with the ALIAS target record which it is linked to. For
      -- more information and an example, see Creating Alias Resource Record Sets in
      -- the Amazon Route 53 Developer Guide.
    , atHostedZoneId :: !Text
      -- ^ Alias resource record sets only: The value of the hosted zone ID for the
      -- AWS resource. For more information and an example, see Creating Alias
      -- Resource Record Sets in the Amazon Route 53 Developer Guide.
    } deriving (Eq, Show, Generic)

instance ToQuery AliasTarget

instance FromXML AliasTarget where
    fromXMLOptions = xmlOptions

instance ToXML AliasTarget where
    toXMLOptions = xmlOptions

-- | Regional resource record sets only: Among resource record sets that have
-- the same combination of DNS name and type, a value that specifies the AWS
-- region for the current resource record set.
data ResourceRecordSetRegion
    = ResourceRecordSetRegionAPNORTHEAST1
    | ResourceRecordSetRegionAPSOUTHEAST1
    | ResourceRecordSetRegionAPSOUTHEAST2
    | ResourceRecordSetRegionEUWEST1
    | ResourceRecordSetRegionSAEAST1
    | ResourceRecordSetRegionUSEAST1
    | ResourceRecordSetRegionUSWEST1
    | ResourceRecordSetRegionUSWEST2
      deriving (Eq, Ord, Generic)

instance Hashable ResourceRecordSetRegion

instance FromText ResourceRecordSetRegion where
    fromText "ap-northeast-1" = Right ResourceRecordSetRegionAPNORTHEAST1
    fromText "ap-southeast-1" = Right ResourceRecordSetRegionAPSOUTHEAST1
    fromText "ap-southeast-2" = Right ResourceRecordSetRegionAPSOUTHEAST2
    fromText "eu-west-1" = Right ResourceRecordSetRegionEUWEST1
    fromText "sa-east-1" = Right ResourceRecordSetRegionSAEAST1
    fromText "us-east-1" = Right ResourceRecordSetRegionUSEAST1
    fromText "us-west-1" = Right ResourceRecordSetRegionUSWEST1
    fromText "us-west-2" = Right ResourceRecordSetRegionUSWEST2
    fromText e = fromTextFail $ "Unrecognised ResourceRecordSetRegion: " <> e

instance Read ResourceRecordSetRegion where
    readsPrec _ = fromTextRead

instance ToText ResourceRecordSetRegion where
    toText ResourceRecordSetRegionAPNORTHEAST1 = "ap-northeast-1"
    toText ResourceRecordSetRegionAPSOUTHEAST1 = "ap-southeast-1"
    toText ResourceRecordSetRegionAPSOUTHEAST2 = "ap-southeast-2"
    toText ResourceRecordSetRegionEUWEST1 = "eu-west-1"
    toText ResourceRecordSetRegionSAEAST1 = "sa-east-1"
    toText ResourceRecordSetRegionUSEAST1 = "us-east-1"
    toText ResourceRecordSetRegionUSWEST1 = "us-west-1"
    toText ResourceRecordSetRegionUSWEST2 = "us-west-2"

instance Show ResourceRecordSetRegion where
    show = toTextShow

instance ToQuery ResourceRecordSetRegion where
    toQuery = toTextQuery

instance FromXML ResourceRecordSetRegion where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ResourceRecordSetRegion where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Failover resource record sets only: Among resource record sets that have
-- the same combination of DNS name and type, a value that indicates whether
-- the current resource record set is a primary or secondary resource record
-- set. A failover set may contain at most one resource record set marked as
-- primary and one resource record set marked as secondary. A resource record
-- set marked as primary will be returned if any of the following are true:
-- (1) an associated health check is passing, (2) if the resource record set
-- is an alias with the evaluate target health and at least one target
-- resource record set is healthy, (3) both the primary and secondary resource
-- record set are failing health checks or (4) there is no secondary resource
-- record set. A secondary resource record set will be returned if: (1) the
-- primary is failing a health check and either the secondary is passing a
-- health check or has no associated health check, or (2) there is no primary
-- resource record set. Valid values: PRIMARY | SECONDARY.
data ResourceRecordSetFailover
    = ResourceRecordSetFailoverPRIMARY
    | ResourceRecordSetFailoverSECONDARY
      deriving (Eq, Ord, Generic)

instance Hashable ResourceRecordSetFailover

instance FromText ResourceRecordSetFailover where
    fromText "PRIMARY" = Right ResourceRecordSetFailoverPRIMARY
    fromText "SECONDARY" = Right ResourceRecordSetFailoverSECONDARY
    fromText e = fromTextFail $ "Unrecognised ResourceRecordSetFailover: " <> e

instance Read ResourceRecordSetFailover where
    readsPrec _ = fromTextRead

instance ToText ResourceRecordSetFailover where
    toText ResourceRecordSetFailoverPRIMARY = "PRIMARY"
    toText ResourceRecordSetFailoverSECONDARY = "SECONDARY"

instance Show ResourceRecordSetFailover where
    show = toTextShow

instance ToQuery ResourceRecordSetFailover where
    toQuery = toTextQuery

instance FromXML ResourceRecordSetFailover where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ResourceRecordSetFailover where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of the current resource record set.
data RRType
    = RRTypeA
    | RRTypeAAAA
    | RRTypeCNAME
    | RRTypeMX
    | RRTypeNS
    | RRTypePTR
    | RRTypeSOA
    | RRTypeSPF
    | RRTypeSRV
    | RRTypeTXT
      deriving (Eq, Ord, Generic)

instance Hashable RRType

instance FromText RRType where
    fromText "A" = Right RRTypeA
    fromText "AAAA" = Right RRTypeAAAA
    fromText "CNAME" = Right RRTypeCNAME
    fromText "MX" = Right RRTypeMX
    fromText "NS" = Right RRTypeNS
    fromText "PTR" = Right RRTypePTR
    fromText "SOA" = Right RRTypeSOA
    fromText "SPF" = Right RRTypeSPF
    fromText "SRV" = Right RRTypeSRV
    fromText "TXT" = Right RRTypeTXT
    fromText e = fromTextFail $ "Unrecognised RRType: " <> e

instance Read RRType where
    readsPrec _ = fromTextRead

instance ToText RRType where
    toText RRTypeA = "A"
    toText RRTypeAAAA = "AAAA"
    toText RRTypeCNAME = "CNAME"
    toText RRTypeMX = "MX"
    toText RRTypeNS = "NS"
    toText RRTypePTR = "PTR"
    toText RRTypeSOA = "SOA"
    toText RRTypeSPF = "SPF"
    toText RRTypeSRV = "SRV"
    toText RRTypeTXT = "TXT"

instance Show RRType where
    show = toTextShow

instance ToQuery RRType where
    toQuery = toTextQuery

instance FromXML RRType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML RRType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of health check to be performed. Currently supported protocols are
-- TCP and HTTP.
data HealthCheckType
    = HealthCheckTypeHTTP
    | HealthCheckTypeTCP
      deriving (Eq, Ord, Generic)

instance Hashable HealthCheckType

instance FromText HealthCheckType where
    fromText "HTTP" = Right HealthCheckTypeHTTP
    fromText "TCP" = Right HealthCheckTypeTCP
    fromText e = fromTextFail $ "Unrecognised HealthCheckType: " <> e

instance Read HealthCheckType where
    readsPrec _ = fromTextRead

instance ToText HealthCheckType where
    toText HealthCheckTypeHTTP = "HTTP"
    toText HealthCheckTypeTCP = "TCP"

instance Show HealthCheckType where
    show = toTextShow

instance ToQuery HealthCheckType where
    toQuery = toTextQuery

instance FromXML HealthCheckType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML HealthCheckType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The current state of the request. PENDING indicates that this request has
-- not yet been applied to all Amazon Route 53 DNS servers. Valid Values:
-- PENDING | INSYNC.
data ChangeStatus
    = ChangeStatusINSYNC
    | ChangeStatusPENDING
      deriving (Eq, Ord, Generic)

instance Hashable ChangeStatus

instance FromText ChangeStatus where
    fromText "INSYNC" = Right ChangeStatusINSYNC
    fromText "PENDING" = Right ChangeStatusPENDING
    fromText e = fromTextFail $ "Unrecognised ChangeStatus: " <> e

instance Read ChangeStatus where
    readsPrec _ = fromTextRead

instance ToText ChangeStatus where
    toText ChangeStatusINSYNC = "INSYNC"
    toText ChangeStatusPENDING = "PENDING"

instance Show ChangeStatus where
    show = toTextShow

instance ToQuery ChangeStatus where
    toQuery = toTextQuery

instance FromXML ChangeStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ChangeStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The action to perform. Valid values: CREATE | DELETE.
data ChangeAction
    = ChangeActionCREATE
    | ChangeActionDELETE
      deriving (Eq, Ord, Generic)

instance Hashable ChangeAction

instance FromText ChangeAction where
    fromText "CREATE" = Right ChangeActionCREATE
    fromText "DELETE" = Right ChangeActionDELETE
    fromText e = fromTextFail $ "Unrecognised ChangeAction: " <> e

instance Read ChangeAction where
    readsPrec _ = fromTextRead

instance ToText ChangeAction where
    toText ChangeActionCREATE = "CREATE"
    toText ChangeActionDELETE = "DELETE"

instance Show ChangeAction where
    show = toTextShow

instance ToQuery ChangeAction where
    toQuery = toTextQuery

instance FromXML ChangeAction where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ChangeAction where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
