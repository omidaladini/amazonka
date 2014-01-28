{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFormation.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudFormation.Service

-- | The TemplateParameter data type.
data TemplateParameter = TemplateParameter
    { tpDefaultValue :: Maybe Text
      -- ^ The default value associated with the parameter.
    , tpDescription :: Maybe Text
      -- ^ User defined description associated with the parameter.
    , tpNoEcho :: Maybe Bool
      -- ^ Flag indicating whether the parameter should be displayed as plain text in
      -- logs and UIs.
    , tpParameterKey :: Maybe Text
      -- ^ The name associated with the parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery TemplateParameter

instance FromXML TemplateParameter where
    fromXMLOptions = xmlOptions

instance ToXML TemplateParameter where
    toXMLOptions = xmlOptions

-- | The Tag type is used by CreateStack in the Tags parameter. It allows you to
-- specify a key/value pair that can be used to store information related to
-- cost allocation for an AWS CloudFormation stack.
data Tag = Tag
    { tKey :: Maybe Text
      -- ^ Required. A string used to identify this tag. You can specify a maximum of
      -- 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have
      -- the reserved prefix: aws:.
    , tValue :: Maybe Text
      -- ^ Required. A string containing the value for this tag. You can specify a
      -- maximum of 256 characters for a tag value.
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

instance ToXML Tag where
    toXMLOptions = xmlOptions

-- | The StackSummary Data Type.
data StackSummary = StackSummary
    { ssCreationTime :: !UTCTime
      -- ^ The time the stack was created.
    , ssDeletionTime :: Maybe UTCTime
      -- ^ The time the stack was deleted.
    , ssLastUpdatedTime :: Maybe UTCTime
      -- ^ The time the stack was last updated. This field will only be returned if
      -- the stack has been updated at least once.
    , ssStackId :: Maybe Text
      -- ^ Unique stack identifier.
    , ssStackName :: !Text
      -- ^ The name associated with the stack.
    , ssStackStatus :: !StackStatus
      -- ^ The current status of the stack.
    , ssStackStatusReason :: Maybe Text
      -- ^ Success/Failure message associated with the stack status.
    , ssTemplateDescription :: Maybe Text
      -- ^ The template description of the template used to create the stack.
    } deriving (Eq, Show, Generic)

instance ToQuery StackSummary

instance FromXML StackSummary where
    fromXMLOptions = xmlOptions

instance ToXML StackSummary where
    toXMLOptions = xmlOptions

-- | Contains high-level information about the specified stack resource.
data StackResourceSummary = StackResourceSummary
    { srsLastUpdatedTimestamp :: !UTCTime
      -- ^ Time the status was updated.
    , srsLogicalResourceId :: !Text
      -- ^ The logical name of the resource specified in the template.
    , srsPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical instance ID of
      -- the resource.
    , srsResourceStatus :: !ResourceStatus
      -- ^ Current status of the resource.
    , srsResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , srsResourceType :: !Text
      -- ^ Type of the resource. (For more information, go to the AWS CloudFormation
      -- User Guide.).
    } deriving (Eq, Show, Generic)

instance ToQuery StackResourceSummary

instance FromXML StackResourceSummary where
    fromXMLOptions = xmlOptions

instance ToXML StackResourceSummary where
    toXMLOptions = xmlOptions

-- | A StackResourceDetail structure containing the description of the specified
-- resource in the specified stack.
data StackResourceDetail = StackResourceDetail
    { srdDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    , srdLastUpdatedTimestamp :: !UTCTime
      -- ^ Time the status was updated.
    , srdLogicalResourceId :: !Text
      -- ^ The logical name of the resource specified in the template.
    , srdMetadata :: Maybe Text
      -- ^ The JSON format content of the Metadata attribute declared for the
      -- resource. For more information, see Metadata Attribute in the AWS
      -- CloudFormation User Guide.
    , srdPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical instance ID of
      -- a resource supported by AWS CloudFormation.
    , srdResourceStatus :: !ResourceStatus
      -- ^ Current status of the resource.
    , srdResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , srdResourceType :: !Text
      -- ^ Type of the resource. (For more information, go to the AWS CloudFormation
      -- User Guide.).
    , srdStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , srdStackName :: Maybe Text
      -- ^ The name associated with the stack.
    } deriving (Eq, Show, Generic)

instance ToQuery StackResourceDetail

instance FromXML StackResourceDetail where
    fromXMLOptions = xmlOptions

instance ToXML StackResourceDetail where
    toXMLOptions = xmlOptions

-- | The StackResource data type.
data StackResource = StackResource
    { srDescription :: Maybe Text
      -- ^ User defined description associated with the resource.
    , srLogicalResourceId :: !Text
      -- ^ The logical name of the resource specified in the template.
    , srPhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier that corresponds to a physical instance ID of
      -- a resource supported by AWS CloudFormation.
    , srResourceStatus :: !ResourceStatus
      -- ^ Current status of the resource.
    , srResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , srResourceType :: !Text
      -- ^ Type of the resource. (For more information, go to the AWS CloudFormation
      -- User Guide.).
    , srStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , srStackName :: Maybe Text
      -- ^ The name associated with the stack.
    , srTimestamp :: !UTCTime
      -- ^ Time the status was updated.
    } deriving (Eq, Show, Generic)

instance ToQuery StackResource

instance FromXML StackResource where
    fromXMLOptions = xmlOptions

instance ToXML StackResource where
    toXMLOptions = xmlOptions

-- | The StackEvent data type.
data StackEvent = StackEvent
    { seEventId :: !Text
      -- ^ The unique ID of this event.
    , seLogicalResourceId :: Maybe Text
      -- ^ The logical name of the resource specified in the template.
    , sePhysicalResourceId :: Maybe Text
      -- ^ The name or unique identifier associated with the physical instance of the
      -- resource.
    , seResourceProperties :: Maybe Text
      -- ^ BLOB of the properties used to create the resource.
    , seResourceStatus :: Maybe ResourceStatus
      -- ^ Current status of the resource.
    , seResourceStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the resource.
    , seResourceType :: Maybe Text
      -- ^ Type of the resource. (For more information, go to the AWS CloudFormation
      -- User Guide.).
    , seStackId :: !Text
      -- ^ The unique ID name of the instance of the stack.
    , seStackName :: !Text
      -- ^ The name associated with a stack.
    , seTimestamp :: !UTCTime
      -- ^ Time the status was updated.
    } deriving (Eq, Show, Generic)

instance ToQuery StackEvent

instance FromXML StackEvent where
    fromXMLOptions = xmlOptions

instance ToXML StackEvent where
    toXMLOptions = xmlOptions

-- | The Stack data type.
data Stack = Stack
    { sCapabilities :: [Capability]
      -- ^ The capabilities allowed in the stack.
    , sCreationTime :: !UTCTime
      -- ^ Time at which the stack was created.
    , sDescription :: Maybe Text
      -- ^ User defined description associated with the stack.
    , sDisableRollback :: Maybe Bool
      -- ^ Boolean to enable or disable rollback on stack creation failures: true:
      -- disable rollback false: enable rollback.
    , sLastUpdatedTime :: Maybe UTCTime
      -- ^ The time the stack was last updated. This field will only be returned if
      -- the stack has been updated at least once.
    , sNotificationARNs :: [Text]
      -- ^ SNS topic ARNs to which stack related events are published.
    , sOutputs :: [Output]
      -- ^ A list of output structures.
    , sParameters :: [Parameter]
      -- ^ A list of Parameter structures.
    , sStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    , sStackName :: !Text
      -- ^ The name associated with the stack.
    , sStackStatus :: !StackStatus
      -- ^ Current status of the stack.
    , sStackStatusReason :: Maybe Text
      -- ^ Success/failure message associated with the stack status.
    , sTags :: [Tag]
      -- ^ A list of Tags that specify cost allocation information for the stack.
    , sTimeoutInMinutes :: Maybe Int
      -- ^ The amount of time within which stack creation should complete.
    } deriving (Eq, Show, Generic)

instance ToQuery Stack

instance FromXML Stack where
    fromXMLOptions = xmlOptions

instance ToXML Stack where
    toXMLOptions = xmlOptions

-- | The Parameter data type.
data Parameter = Parameter
    { pParameterKey :: Maybe Text
      -- ^ The key associated with the parameter.
    , pParameterValue :: Maybe Text
      -- ^ The value associated with the parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery Parameter

instance FromXML Parameter where
    fromXMLOptions = xmlOptions

instance ToXML Parameter where
    toXMLOptions = xmlOptions

-- | The Output data type.
data Output = Output
    { oDescription :: Maybe Text
      -- ^ User defined description associated with the output.
    , oOutputKey :: Maybe Text
      -- ^ The key associated with the output.
    , oOutputValue :: Maybe Text
      -- ^ The value associated with the output.
    } deriving (Eq, Show, Generic)

instance ToQuery Output

instance FromXML Output where
    fromXMLOptions = xmlOptions

instance ToXML Output where
    toXMLOptions = xmlOptions

-- | Current status of the stack.
data StackStatus
    = StackStatusCREATE_COMPLETE
    | StackStatusCREATE_FAILED
    | StackStatusCREATE_IN_PROGRESS
    | StackStatusDELETE_COMPLETE
    | StackStatusDELETE_FAILED
    | StackStatusDELETE_IN_PROGRESS
    | StackStatusROLLBACK_COMPLETE
    | StackStatusROLLBACK_FAILED
    | StackStatusROLLBACK_IN_PROGRESS
    | StackStatusUPDATE_COMPLETE
    | StackStatusUPDATE_COMPLETE_CLEANUP_IN_PROGRESS
    | StackStatusUPDATE_IN_PROGRESS
    | StackStatusUPDATE_ROLLBACK_COMPLETE
    | StackStatusUPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS
    | StackStatusUPDATE_ROLLBACK_FAILED
    | StackStatusUPDATE_ROLLBACK_IN_PROGRESS
      deriving (Eq, Ord, Generic)

instance Hashable StackStatus

instance FromText StackStatus where
    fromText "CREATE_COMPLETE" = Right StackStatusCREATE_COMPLETE
    fromText "CREATE_FAILED" = Right StackStatusCREATE_FAILED
    fromText "CREATE_IN_PROGRESS" = Right StackStatusCREATE_IN_PROGRESS
    fromText "DELETE_COMPLETE" = Right StackStatusDELETE_COMPLETE
    fromText "DELETE_FAILED" = Right StackStatusDELETE_FAILED
    fromText "DELETE_IN_PROGRESS" = Right StackStatusDELETE_IN_PROGRESS
    fromText "ROLLBACK_COMPLETE" = Right StackStatusROLLBACK_COMPLETE
    fromText "ROLLBACK_FAILED" = Right StackStatusROLLBACK_FAILED
    fromText "ROLLBACK_IN_PROGRESS" = Right StackStatusROLLBACK_IN_PROGRESS
    fromText "UPDATE_COMPLETE" = Right StackStatusUPDATE_COMPLETE
    fromText "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" = Right StackStatusUPDATE_COMPLETE_CLEANUP_IN_PROGRESS
    fromText "UPDATE_IN_PROGRESS" = Right StackStatusUPDATE_IN_PROGRESS
    fromText "UPDATE_ROLLBACK_COMPLETE" = Right StackStatusUPDATE_ROLLBACK_COMPLETE
    fromText "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" = Right StackStatusUPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS
    fromText "UPDATE_ROLLBACK_FAILED" = Right StackStatusUPDATE_ROLLBACK_FAILED
    fromText "UPDATE_ROLLBACK_IN_PROGRESS" = Right StackStatusUPDATE_ROLLBACK_IN_PROGRESS
    fromText e = fromTextFail $ "Unrecognised StackStatus: " <> e

instance Read StackStatus where
    readsPrec _ = fromTextRead

instance ToText StackStatus where
    toText StackStatusCREATE_COMPLETE = "CREATE_COMPLETE"
    toText StackStatusCREATE_FAILED = "CREATE_FAILED"
    toText StackStatusCREATE_IN_PROGRESS = "CREATE_IN_PROGRESS"
    toText StackStatusDELETE_COMPLETE = "DELETE_COMPLETE"
    toText StackStatusDELETE_FAILED = "DELETE_FAILED"
    toText StackStatusDELETE_IN_PROGRESS = "DELETE_IN_PROGRESS"
    toText StackStatusROLLBACK_COMPLETE = "ROLLBACK_COMPLETE"
    toText StackStatusROLLBACK_FAILED = "ROLLBACK_FAILED"
    toText StackStatusROLLBACK_IN_PROGRESS = "ROLLBACK_IN_PROGRESS"
    toText StackStatusUPDATE_COMPLETE = "UPDATE_COMPLETE"
    toText StackStatusUPDATE_COMPLETE_CLEANUP_IN_PROGRESS = "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
    toText StackStatusUPDATE_IN_PROGRESS = "UPDATE_IN_PROGRESS"
    toText StackStatusUPDATE_ROLLBACK_COMPLETE = "UPDATE_ROLLBACK_COMPLETE"
    toText StackStatusUPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS = "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
    toText StackStatusUPDATE_ROLLBACK_FAILED = "UPDATE_ROLLBACK_FAILED"
    toText StackStatusUPDATE_ROLLBACK_IN_PROGRESS = "UPDATE_ROLLBACK_IN_PROGRESS"

instance Show StackStatus where
    show = toTextShow

instance ToQuery StackStatus where
    toQuery = toTextQuery

instance FromXML StackStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StackStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Current status of the resource.
data ResourceStatus
    = ResourceStatusCREATE_COMPLETE
    | ResourceStatusCREATE_FAILED
    | ResourceStatusCREATE_IN_PROGRESS
    | ResourceStatusDELETE_COMPLETE
    | ResourceStatusDELETE_FAILED
    | ResourceStatusDELETE_IN_PROGRESS
    | ResourceStatusUPDATE_COMPLETE
    | ResourceStatusUPDATE_FAILED
    | ResourceStatusUPDATE_IN_PROGRESS
      deriving (Eq, Ord, Generic)

instance Hashable ResourceStatus

instance FromText ResourceStatus where
    fromText "CREATE_COMPLETE" = Right ResourceStatusCREATE_COMPLETE
    fromText "CREATE_FAILED" = Right ResourceStatusCREATE_FAILED
    fromText "CREATE_IN_PROGRESS" = Right ResourceStatusCREATE_IN_PROGRESS
    fromText "DELETE_COMPLETE" = Right ResourceStatusDELETE_COMPLETE
    fromText "DELETE_FAILED" = Right ResourceStatusDELETE_FAILED
    fromText "DELETE_IN_PROGRESS" = Right ResourceStatusDELETE_IN_PROGRESS
    fromText "UPDATE_COMPLETE" = Right ResourceStatusUPDATE_COMPLETE
    fromText "UPDATE_FAILED" = Right ResourceStatusUPDATE_FAILED
    fromText "UPDATE_IN_PROGRESS" = Right ResourceStatusUPDATE_IN_PROGRESS
    fromText e = fromTextFail $ "Unrecognised ResourceStatus: " <> e

instance Read ResourceStatus where
    readsPrec _ = fromTextRead

instance ToText ResourceStatus where
    toText ResourceStatusCREATE_COMPLETE = "CREATE_COMPLETE"
    toText ResourceStatusCREATE_FAILED = "CREATE_FAILED"
    toText ResourceStatusCREATE_IN_PROGRESS = "CREATE_IN_PROGRESS"
    toText ResourceStatusDELETE_COMPLETE = "DELETE_COMPLETE"
    toText ResourceStatusDELETE_FAILED = "DELETE_FAILED"
    toText ResourceStatusDELETE_IN_PROGRESS = "DELETE_IN_PROGRESS"
    toText ResourceStatusUPDATE_COMPLETE = "UPDATE_COMPLETE"
    toText ResourceStatusUPDATE_FAILED = "UPDATE_FAILED"
    toText ResourceStatusUPDATE_IN_PROGRESS = "UPDATE_IN_PROGRESS"

instance Show ResourceStatus where
    show = toTextShow

instance ToQuery ResourceStatus where
    toQuery = toTextQuery

instance FromXML ResourceStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ResourceStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Determines what action will be taken if stack creation fails. This must be
-- one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either OnFailure
-- or DisableRollback, but not both. Default: ROLLBACK.
data OnFailure
    = OnFailureDELETE
    | OnFailureDO_NOTHING
    | OnFailureROLLBACK
      deriving (Eq, Ord, Generic)

instance Hashable OnFailure

instance FromText OnFailure where
    fromText "DELETE" = Right OnFailureDELETE
    fromText "DO_NOTHING" = Right OnFailureDO_NOTHING
    fromText "ROLLBACK" = Right OnFailureROLLBACK
    fromText e = fromTextFail $ "Unrecognised OnFailure: " <> e

instance Read OnFailure where
    readsPrec _ = fromTextRead

instance ToText OnFailure where
    toText OnFailureDELETE = "DELETE"
    toText OnFailureDO_NOTHING = "DO_NOTHING"
    toText OnFailureROLLBACK = "ROLLBACK"

instance Show OnFailure where
    show = toTextShow

instance ToQuery OnFailure where
    toQuery = toTextQuery

instance FromXML OnFailure where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML OnFailure where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for Capability
data Capability
    = CapabilityCAPABILITY_IAM
      deriving (Eq, Ord, Generic)

instance Hashable Capability

instance FromText Capability where
    fromText "CAPABILITY_IAM" = Right CapabilityCAPABILITY_IAM
    fromText e = fromTextFail $ "Unrecognised Capability: " <> e

instance Read Capability where
    readsPrec _ = fromTextRead

instance ToText Capability where
    toText CapabilityCAPABILITY_IAM = "CAPABILITY_IAM"

instance Show Capability where
    show = toTextShow

instance ToQuery Capability where
    toQuery = toTextQuery

instance FromXML Capability where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Capability where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
