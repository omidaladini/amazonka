{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DataPipeline.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DataPipeline.Service

-- | Defines a validation warning returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation warnings do not prevent pipeline
-- activation. The set of validation warnings that can be returned are defined
-- by AWS Data Pipeline.
data ValidationWarning = ValidationWarning
    { vwid :: Maybe Text
      -- ^ The identifier of the object that contains the validation warning.
    , vwwarnings :: [Text]
      -- ^ A description of the validation warning.
    } deriving (Eq, Show, Generic)

instance FromJSON ValidationWarning
instance ToJSON ValidationWarning

-- | Defines a validation error returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation errors prevent pipeline activation.
-- The set of validation errors that can be returned are defined by AWS Data
-- Pipeline.
data ValidationError = ValidationError
    { veerrors :: [Text]
      -- ^ A description of the validation error.
    , veid :: Maybe Text
      -- ^ The identifier of the object that contains the validation error.
    } deriving (Eq, Show, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError

-- | An instance of PollForTaskResult, which contains an instance of TaskObject.
-- The returned object contains all the information needed to complete the
-- task that is being assigned to the task runner. One of the fields returned
-- in this object is taskId, which contains an identifier for the task being
-- assigned. The calling task runner uses taskId in subsequent calls to
-- ReportTaskProgress and SetTaskStatus.
data TaskObject = TaskObject
    { toattemptId :: Maybe Text
      -- ^ Identifier of the pipeline task attempt object. AWS Data Pipeline uses this
      -- value to track how many times a task is attempted.
    , toobjects :: HashMap Text PipelineObject
      -- ^ Connection information for the location where the task runner will publish
      -- the output of the task.
    , topipelineId :: Maybe Text
      -- ^ Identifier of the pipeline that provided the task.
    , totaskId :: Maybe Text
      -- ^ An internal identifier for the task. This ID is passed to the SetTaskStatus
      -- and ReportTaskProgress actions.
    } deriving (Eq, Show, Generic)

instance FromJSON TaskObject
instance ToJSON TaskObject

-- | A comparision that is used to determine whether a query should return this
-- object.
data Selector = Selector
    { sfieldName :: Maybe Text
      -- ^ The name of the field that the operator will be applied to. The field name
      -- is the "key" portion of the field definition in the pipeline definition
      -- syntax that is used by the AWS Data Pipeline API. If the field is not set
      -- on the object, the condition fails.
    , soperator :: Maybe Operator
      -- ^ Contains a logical operation for comparing the value of a field with a
      -- specified value.
    } deriving (Eq, Show, Generic)

instance FromJSON Selector
instance ToJSON Selector

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
newtype Query = Query
    { qselectors :: [Selector]
      -- ^ List of selectors that define the query. An object must satisfy all of the
      -- selectors to match the query.
    } deriving (Eq, Show, Generic)

instance FromJSON Query
instance ToJSON Query

-- | Contains information about a pipeline object. This can be a logical,
-- physical, or physical attempt pipeline object. The complete set of
-- components of a pipeline defines the pipeline.
data PipelineObject = PipelineObject
    { pofields :: [Field]
      -- ^ Key-value pairs that define the properties of the object.
    , poid :: !Text
      -- ^ Identifier of the object.
    , poname :: !Text
      -- ^ Name of the object.
    } deriving (Eq, Show, Generic)

instance FromJSON PipelineObject
instance ToJSON PipelineObject

-- | Contains the name and identifier of a pipeline.
data PipelineIdName = PipelineIdName
    { pinid :: Maybe Text
      -- ^ Identifier of the pipeline that was assigned by AWS Data Pipeline. This is
      -- a string of the form df-297EG78HU43EEXAMPLE.
    , pinname :: Maybe Text
      -- ^ Name of the pipeline.
    } deriving (Eq, Show, Generic)

instance FromJSON PipelineIdName
instance ToJSON PipelineIdName

-- | Contains pipeline metadata.
data PipelineDescription = PipelineDescription
    { pddescription :: Maybe Text
      -- ^ Description of the pipeline.
    , pdfields :: [Field]
      -- ^ A list of read-only fields that contain metadata about the pipeline:
      -- @userId, @accountId, and @pipelineState.
    , pdname :: !Text
      -- ^ Name of the pipeline.
    , pdpipelineId :: !Text
      -- ^ The pipeline identifier that was assigned by AWS Data Pipeline. This is a
      -- string of the form df-297EG78HU43EEXAMPLE.
    } deriving (Eq, Show, Generic)

instance FromJSON PipelineDescription
instance ToJSON PipelineDescription

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
data Operator = Operator
    { otype :: Maybe OperatorType
      -- ^ The logical operation to be performed: equal (EQ), equal reference
      -- (REF_EQ), less than or equal (LE), greater than or equal (GE), or between
      -- (BETWEEN). Equal reference (REF_EQ) can be used only with reference fields.
      -- The other comparison types can be used only with String fields. The
      -- comparison types you can use apply only to certain object fields, as
      -- detailed below. The comparison operators EQ and REF_EQ act on the following
      -- fields: name @sphere parent @componentParent @instanceParent @status
      -- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime The
      -- comparison operators GE, LE, and BETWEEN act on the following fields:
      -- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime Note
      -- that fields beginning with the at sign (@) are read-only and set by the web
      -- service. When you name fields, you should choose names containing only
      -- alpha-numeric values, as symbols may be reserved by AWS Data Pipeline.
      -- User-defined fields that you add to a pipeline should prefix their name
      -- with the string "my".
    , ovalues :: [Text]
      -- ^ The value that the actual field value will be compared with.
    } deriving (Eq, Show, Generic)

instance FromJSON Operator
instance ToJSON Operator

-- | Identity information for the Amazon EC2 instance that is hosting the task
-- runner. You can get this value by calling the URI,
-- http://169.254.169.254/latest/meta-data/instance-id, from the EC2 instance.
-- For more information, go to Instance Metadata in the Amazon Elastic Compute
-- Cloud User Guide. Passing in this value proves that your task runner is
-- running on an EC2 instance, and ensures the proper AWS Data Pipeline
-- service charges are applied to your pipeline.
data InstanceIdentity = InstanceIdentity
    { iidocument :: Maybe Text
      -- ^ A description of an Amazon EC2 instance that is generated when the instance
      -- is launched and exposed to the instance via the instance metadata service
      -- in the form of a JSON representation of an object.
    , iisignature :: Maybe Text
      -- ^ A signature which can be used to verify the accuracy and authenticity of
      -- the information provided in the instance identity document.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceIdentity
instance ToJSON InstanceIdentity

-- | A key-value pair that describes a property of a pipeline object. The value
-- is specified as either a string value (StringValue) or a reference to
-- another object (RefValue) but not as both.
data Field = Field
    { fkey :: !Text
      -- ^ The field identifier.
    , frefValue :: Maybe Text
      -- ^ The field value, expressed as the identifier of another object.
    , fstringValue :: Maybe Text
      -- ^ The field value, expressed as a String.
    } deriving (Eq, Show, Generic)

instance FromJSON Field
instance ToJSON Field

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.

data TaskStatus
    = TaskStatusFAILED
    | TaskStatusFALSE
    | TaskStatusFINISHED
      deriving (Eq, Ord, Generic)

instance Hashable TaskStatus

instance FromText TaskStatus where
    fromText "FAILED" = Right TaskStatusFAILED
    fromText "FALSE" = Right TaskStatusFALSE
    fromText "FINISHED" = Right TaskStatusFINISHED
    fromText e = fromTextFail $ "Unrecognised TaskStatus: " <> e

instance Read TaskStatus where
    readsPrec _ = fromTextRead

instance ToText TaskStatus where
    toText TaskStatusFAILED = "FAILED"
    toText TaskStatusFALSE = "FALSE"
    toText TaskStatusFINISHED = "FINISHED"

instance Show TaskStatus where
    show = toTextShow

instance FromJSON TaskStatus where
    parseJSON = fromTextJSON "TaskStatus"

instance FromJSON v => FromJSON (HashMap TaskStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON TaskStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap TaskStatus v) where
    toJSON = toTextHashJSON

-- | The logical operation to be performed: equal (EQ), equal reference
-- (REF_EQ), less than or equal (LE), greater than or equal (GE), or between
-- (BETWEEN). Equal reference (REF_EQ) can be used only with reference fields.
-- The other comparison types can be used only with String fields. The
-- comparison types you can use apply only to certain object fields, as
-- detailed below. The comparison operators EQ and REF_EQ act on the following
-- fields: name @sphere parent @componentParent @instanceParent @status
-- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime The
-- comparison operators GE, LE, and BETWEEN act on the following fields:
-- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime Note
-- that fields beginning with the at sign (@) are read-only and set by the web
-- service. When you name fields, you should choose names containing only
-- alpha-numeric values, as symbols may be reserved by AWS Data Pipeline.
-- User-defined fields that you add to a pipeline should prefix their name
-- with the string "my".

data OperatorType
    = OperatorTypeBETWEEN
    | OperatorTypeEQ
    | OperatorTypeGE
    | OperatorTypeLE
    | OperatorTypeREF_EQ
      deriving (Eq, Ord, Generic)

instance Hashable OperatorType

instance FromText OperatorType where
    fromText "BETWEEN" = Right OperatorTypeBETWEEN
    fromText "EQ" = Right OperatorTypeEQ
    fromText "GE" = Right OperatorTypeGE
    fromText "LE" = Right OperatorTypeLE
    fromText "REF_EQ" = Right OperatorTypeREF_EQ
    fromText e = fromTextFail $ "Unrecognised OperatorType: " <> e

instance Read OperatorType where
    readsPrec _ = fromTextRead

instance ToText OperatorType where
    toText OperatorTypeBETWEEN = "BETWEEN"
    toText OperatorTypeEQ = "EQ"
    toText OperatorTypeGE = "GE"
    toText OperatorTypeLE = "LE"
    toText OperatorTypeREF_EQ = "REF_EQ"

instance Show OperatorType where
    show = toTextShow

instance FromJSON OperatorType where
    parseJSON = fromTextJSON "OperatorType"

instance FromJSON v => FromJSON (HashMap OperatorType v) where
    parseJSON = fromTextHashJSON

instance ToJSON OperatorType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap OperatorType v) where
    toJSON = toTextHashJSON
