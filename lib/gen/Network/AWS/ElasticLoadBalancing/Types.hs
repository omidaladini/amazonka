{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticLoadBalancing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticLoadBalancing.Types where

import Data.Monoid
import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
data SourceSecurityGroup = SourceSecurityGroup
    { ssgGroupName :: Maybe Text
      -- ^ Name of the source security group. Use this value for the --source-group
      -- parameter of the ec2-authorize command in the Amazon EC2 command line tool.
    , ssgOwnerAlias :: Maybe Text
      -- ^ Owner of the source security group. Use this value for the
      -- --source-group-user parameter of the ec2-authorize command in the Amazon
      -- EC2 command line tool.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceSecurityGroup

instance FromXML SourceSecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML SourceSecurityGroup where
    toXMLOptions = xmlOptions

-- | The PolicyTypeDescription data type.
data PolicyTypeDescription = PolicyTypeDescription
    { ptdDescription :: Maybe Text
      -- ^ A human-readable description of the policy type.
    , ptdPolicyAttributeTypeDescriptions :: [PolicyAttributeTypeDescription]
      -- ^ The description of the policy attributes associated with the load balancer
      -- policies defined by the Elastic Load Balancing service.
    , ptdPolicyTypeName :: Maybe Text
      -- ^ The name of the policy type.
    } deriving (Eq, Show, Generic)

instance ToQuery PolicyTypeDescription

instance FromXML PolicyTypeDescription where
    fromXMLOptions = xmlOptions

instance ToXML PolicyTypeDescription where
    toXMLOptions = xmlOptions

-- | The PolicyDescription data type.
data PolicyDescription = PolicyDescription
    { pdPolicyAttributeDescriptions :: [PolicyAttributeDescription]
      -- ^ A list of policy attribute description structures.
    , pdPolicyName :: Maybe Text
      -- ^ The name of the policy associated with the load balancer.
    , pdPolicyTypeName :: Maybe Text
      -- ^ The name of the policy type associated with the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery PolicyDescription

instance FromXML PolicyDescription where
    fromXMLOptions = xmlOptions

instance ToXML PolicyDescription where
    toXMLOptions = xmlOptions

-- | The PolicyAttributeTypeDescription data type. This data type is used to
-- describe values that are acceptable for the policy attribute.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { patdAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy type.
    , patdAttributeType :: Maybe Text
      -- ^ The type of attribute. For example, Boolean, Integer, etc.
    , patdCardinality :: Maybe Text
      -- ^ The cardinality of the attribute. Valid Values: ONE(1) : Single value
      -- required ZERO_OR_ONE(0..1) : Up to one value can be supplied
      -- ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
      -- ONE_OR_MORE(1..*0) : Required. Multiple values are allowed.
    , patdDefaultValue :: Maybe Text
      -- ^ The default value of the attribute, if applicable.
    , patdDescription :: Maybe Text
      -- ^ A human-readable description of the attribute.
    } deriving (Eq, Show, Generic)

instance ToQuery PolicyAttributeTypeDescription

instance FromXML PolicyAttributeTypeDescription where
    fromXMLOptions = xmlOptions

instance ToXML PolicyAttributeTypeDescription where
    toXMLOptions = xmlOptions

-- | The PolicyAttributeDescription data type. This data type is used to
-- describe the attributes and values associated with a policy.
data PolicyAttributeDescription = PolicyAttributeDescription
    { padAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , padAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Eq, Show, Generic)

instance ToQuery PolicyAttributeDescription

instance FromXML PolicyAttributeDescription where
    fromXMLOptions = xmlOptions

instance ToXML PolicyAttributeDescription where
    toXMLOptions = xmlOptions

-- | The PolicyAttribute data type. This data type contains a key/value pair
-- that defines properties of a specific policy.
data PolicyAttribute = PolicyAttribute
    { paAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , paAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Eq, Show, Generic)

instance ToQuery PolicyAttribute

instance FromXML PolicyAttribute where
    fromXMLOptions = xmlOptions

instance ToXML PolicyAttribute where
    toXMLOptions = xmlOptions

-- | Provides a list of policies defined for the load balancer.
data Policies = Policies
    { pAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
      -- ^ A list of the AppCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , pLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
      -- ^ A list of LBCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , pOtherPolicies :: [Text]
      -- ^ A list of policy names other than the stickiness policies.
    } deriving (Eq, Show, Generic)

instance ToQuery Policies

instance FromXML Policies where
    fromXMLOptions = xmlOptions

instance ToXML Policies where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of DescribeLoadBalancers.
data LoadBalancerDescription = LoadBalancerDescription
    { lbdAvailabilityZones :: [Text]
      -- ^ Specifies a list of Availability Zones.
    , lbdBackendServerDescriptions :: [BackendServerDescription]
      -- ^ Contains a list of back-end server descriptions.
    , lbdCanonicalHostedZoneName :: Maybe Text
      -- ^ Provides the name of the Amazon Route 53 hosted zone that is associated
      -- with the load balancer. For information on how to associate your load
      -- balancer with a hosted zone, go to Using Domain Names With Elastic Load
      -- Balancing in the Elastic Load Balancing Developer Guide.
    , lbdCanonicalHostedZoneNameID :: Maybe Text
      -- ^ Provides the ID of the Amazon Route 53 hosted zone name that is associated
      -- with the load balancer. For information on how to associate or disassociate
      -- your load balancer with a hosted zone, go to Using Domain Names With
      -- Elastic Load Balancing in the Elastic Load Balancing Developer Guide.
    , lbdCreatedTime :: Maybe UTCTime
      -- ^ Provides the date and time the load balancer was created.
    , lbdDNSName :: Maybe Text
      -- ^ Specifies the external DNS name associated with the load balancer.
    , lbdHealthCheck :: Maybe HealthCheck
      -- ^ Specifies information regarding the various health probes conducted on the
      -- load balancer.
    , lbdInstances :: [Instance]
      -- ^ Provides a list of EC2 instance IDs for the load balancer.
    , lbdListenerDescriptions :: [ListenerDescription]
      -- ^ LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and PolicyNames
      -- are returned in a list of tuples in the ListenerDescriptions element.
    , lbdLoadBalancerName :: Maybe Text
      -- ^ Specifies the name associated with the load balancer.
    , lbdPolicies :: Maybe Policies
      -- ^ Provides a list of policies defined for the load balancer.
    , lbdScheme :: Maybe Text
      -- ^ Specifies the type of load balancer. If the Scheme is internet-facing, the
      -- load balancer has a publicly resolvable DNS name that resolves to public IP
      -- addresses. If the Scheme is internal, the load balancer has a publicly
      -- resolvable DNS name that resolves to private IP addresses. This option is
      -- only available for load balancers attached to an Amazon VPC.
    , lbdSecurityGroups :: [Text]
      -- ^ The security groups the load balancer is a member of (VPC only).
    , lbdSourceSecurityGroup :: Maybe SourceSecurityGroup
      -- ^ The security group that you can use as part of your inbound rules for your
      -- load balancer's back-end Amazon EC2 application instances. To only allow
      -- traffic from load balancers, add a security group rule to your back end
      -- instance that specifies this source security group as the inbound source.
    , lbdSubnets :: [Text]
      -- ^ Provides a list of VPC subnet IDs for the load balancer.
    , lbdVpcId :: Maybe Text
      -- ^ Provides the ID of the VPC attached to the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery LoadBalancerDescription

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions

instance ToXML LoadBalancerDescription where
    toXMLOptions = xmlOptions

-- | Attributes of the load balancer.
newtype LoadBalancerAttributes = LoadBalancerAttributes
    { lbaCrossZoneLoadBalancing :: Maybe CrossZoneLoadBalancing
      -- ^ The name of the load balancer attribute.
    } deriving (Eq, Show, Generic)

instance ToQuery LoadBalancerAttributes

instance FromXML LoadBalancerAttributes where
    fromXMLOptions = xmlOptions

instance ToXML LoadBalancerAttributes where
    toXMLOptions = xmlOptions

-- | The ListenerDescription data type.
data ListenerDescription = ListenerDescription
    { ldListener :: Maybe Listener
      -- ^ The Listener data type.
    , ldPolicyNames :: [Text]
      -- ^ A list of policies enabled for this listener. An empty list indicates that
      -- no policies are enabled.
    } deriving (Eq, Show, Generic)

instance ToQuery ListenerDescription

instance FromXML ListenerDescription where
    fromXMLOptions = xmlOptions

instance ToXML ListenerDescription where
    toXMLOptions = xmlOptions

-- | The Listener data type.
data Listener = Listener
    { lInstancePort :: !Int
      -- ^ Specifies the TCP port on which the instance server is listening. This
      -- property cannot be modified for the life of the load balancer.
    , lInstanceProtocol :: Maybe Text
      -- ^ Specifies the protocol to use for routing traffic to back-end instances -
      -- HTTP, HTTPS, TCP, or SSL. This property cannot be modified for the life of
      -- the load balancer. If the front-end protocol is HTTP or HTTPS,
      -- InstanceProtocol has to be at the same protocol layer, i.e., HTTP or HTTPS.
      -- Likewise, if the front-end protocol is TCP or SSL, InstanceProtocol has to
      -- be TCP or SSL. If there is another listener with the same InstancePort
      -- whose InstanceProtocol is secure, i.e., HTTPS or SSL, the listener's
      -- InstanceProtocol has to be secure, i.e., HTTPS or SSL. If there is another
      -- listener with the same InstancePort whose InstanceProtocol is HTTP or TCP,
      -- the listener's InstanceProtocol must be either HTTP or TCP.
    , lLoadBalancerPort :: !Int
      -- ^ Specifies the external load balancer port number. This property cannot be
      -- modified for the life of the load balancer.
    , lProtocol :: !Text
      -- ^ Specifies the load balancer transport protocol to use for routing - HTTP,
      -- HTTPS, TCP or SSL. This property cannot be modified for the life of the
      -- load balancer.
    , lSSLCertificateId :: Maybe Text
      -- ^ The ARN string of the server certificate. To get the ARN of the server
      -- certificate, call the AWS Identity and Access Management
      -- UploadServerCertificate API.
    } deriving (Eq, Show, Generic)

instance ToQuery Listener

instance FromXML Listener where
    fromXMLOptions = xmlOptions

instance ToXML Listener where
    toXMLOptions = xmlOptions

-- | The LBCookieStickinessPolicy data type.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { lbcspCookieExpirationPeriod :: Maybe Integer
      -- ^ The time period in seconds after which the cookie should be considered
      -- stale. Not specifying this parameter indicates that the stickiness session
      -- will last for the duration of the browser session.
    , lbcspPolicyName :: Maybe Text
      -- ^ The name for the policy being created. The name must be unique within the
      -- set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery LBCookieStickinessPolicy

instance FromXML LBCookieStickinessPolicy where
    fromXMLOptions = xmlOptions

instance ToXML LBCookieStickinessPolicy where
    toXMLOptions = xmlOptions

-- | The InstanceState data type.
data InstanceState = InstanceState
    { isDescription :: Maybe Text
      -- ^ Provides a description of the instance state.
    , isInstanceId :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    , isReasonCode :: Maybe Text
      -- ^ Provides information about the cause of OutOfService instances.
      -- Specifically, it indicates whether the cause is Elastic Load Balancing or
      -- the instance behind the load balancer. Valid value: ELB|Instance|N/A.
    , isState :: Maybe Text
      -- ^ Specifies the current state of the instance. Valid value:
      -- InService|OutOfService.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceState

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions

instance ToXML InstanceState where
    toXMLOptions = xmlOptions

-- | The Instance data type.
newtype Instance = Instance
    { iInstanceId :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    } deriving (Eq, Show, Generic)

instance ToQuery Instance

instance FromXML Instance where
    fromXMLOptions = xmlOptions

instance ToXML Instance where
    toXMLOptions = xmlOptions

-- | Specifies information regarding the various health probes conducted on the
-- load balancer.
data HealthCheck = HealthCheck
    { hcHealthyThreshold :: !Int
      -- ^ Specifies the number of consecutive health probe successes required before
      -- moving the instance to the Healthy state.
    , hcInterval :: !Int
      -- ^ Specifies the approximate interval, in seconds, between health checks of an
      -- individual instance.
    , hcTarget :: !Text
      -- ^ Specifies the instance being checked. The protocol is either TCP, HTTP,
      -- HTTPS, or SSL. The range of valid ports is one (1) through 65535. TCP is
      -- the default, specified as a TCP: port pair, for example "TCP:5000". In this
      -- case a healthcheck simply attempts to open a TCP connection to the instance
      -- on the specified port. Failure to connect within the configured timeout is
      -- considered unhealthy. SSL is also specified as SSL: port pair, for example,
      -- SSL:5000. For HTTP or HTTPS protocol, the situation is different. You have
      -- to include a ping path in the string. HTTP is specified as a
      -- HTTP:port;/;PathToPing; grouping, for example
      -- "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued
      -- to the instance on the given port and path. Any answer other than "200 OK"
      -- within the timeout period is considered unhealthy. The total length of the
      -- HTTP ping target needs to be 1024 16-bit Unicode characters or less.
    , hcTimeout :: !Int
      -- ^ Specifies the amount of time, in seconds, during which no response means a
      -- failed health probe. This value must be less than the Interval value.
    , hcUnhealthyThreshold :: !Int
      -- ^ Specifies the number of consecutive health probe failures required before
      -- moving the instance to the Unhealthy state.
    } deriving (Eq, Show, Generic)

instance ToQuery HealthCheck

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions

instance ToXML HealthCheck where
    toXMLOptions = xmlOptions

-- | The name of the load balancer attribute.
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing
    { czlbEnabled :: Bool
      -- ^ Specifies whether cross-zone load balancing is enabled for the load
      -- balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery CrossZoneLoadBalancing

instance FromXML CrossZoneLoadBalancing where
    fromXMLOptions = xmlOptions

instance ToXML CrossZoneLoadBalancing where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the DescribeLoadBalancers
-- action to describe the configuration of the back-end server.
data BackendServerDescription = BackendServerDescription
    { bsdInstancePort :: Maybe Int
      -- ^ Provides the port on which the back-end server is listening.
    , bsdPolicyNames :: [Text]
      -- ^ Provides a list of policy names enabled for the back-end server.
    } deriving (Eq, Show, Generic)

instance ToQuery BackendServerDescription

instance FromXML BackendServerDescription where
    fromXMLOptions = xmlOptions

instance ToXML BackendServerDescription where
    toXMLOptions = xmlOptions

-- | The AppCookieStickinessPolicy data type.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { acspCookieName :: Maybe Text
      -- ^ The name of the application cookie used for stickiness.
    , acspPolicyName :: Maybe Text
      -- ^ The mnemonic name for the policy being created. The name must be unique
      -- within a set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery AppCookieStickinessPolicy

instance FromXML AppCookieStickinessPolicy where
    fromXMLOptions = xmlOptions

instance ToXML AppCookieStickinessPolicy where
    toXMLOptions = xmlOptions
