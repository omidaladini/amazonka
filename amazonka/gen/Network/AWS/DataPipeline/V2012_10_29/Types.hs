{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Data Pipeline is a web service that you can use to automate the
-- movement and transformation of data. With AWS Data Pipeline, you can define
-- data-driven workflows, so that tasks can be dependent on the successful
-- completion of previous tasks.
module Network.AWS.DataPipeline.V2012_10_29.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-10-29@) of the
-- @AWS Data Pipeline@ service.
data DataPipeline deriving (Typeable)

instance AWSService DataPipeline where
    type Sg DataPipeline = V4
    data Er DataPipeline
        = DataPipelineClient HttpException
        | DataPipelineSerializer String
        | DataPipelineService String
        | InternalServiceError
            { _iseMessage :: Maybe Text
            }
        | InvalidRequestException
            { _ireMessage :: Maybe Text
            }
        | PipelineDeletedException
            { _pdeMessage :: Maybe Text
            }
        | PipelineNotFoundException
            { _pnfeMessage :: Maybe Text
            }
        | TaskNotFoundException
            { _tnfeMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "datapipeline"
        , _svcVersion  = "2012-10-29"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er DataPipeline)
deriving instance Generic (Er DataPipeline)

instance AWSError (Er DataPipeline) where
    awsError = const "DataPipelineError"

instance AWSServiceError (Er DataPipeline) where
    serviceError    = DataPipelineService
    clientError     = DataPipelineClient
    serializerError = DataPipelineSerializer

instance Exception (Er DataPipeline)

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
    = OperatorTypeBetween -- ^ BETWEEN
    | OperatorTypeEq -- ^ EQ
    | OperatorTypeGe -- ^ GE
    | OperatorTypeLe -- ^ LE
    | OperatorTypeRefEq -- ^ REF_EQ
      deriving (Eq, Show, Generic)

instance Hashable OperatorType

instance FromText OperatorType where
    parser = match "BETWEEN" OperatorTypeBetween
         <|> match "EQ" OperatorTypeEq
         <|> match "GE" OperatorTypeGe
         <|> match "LE" OperatorTypeLe
         <|> match "REF_EQ" OperatorTypeRefEq

instance ToText OperatorType where
    toText OperatorTypeBetween = "BETWEEN"
    toText OperatorTypeEq = "EQ"
    toText OperatorTypeGe = "GE"
    toText OperatorTypeLe = "LE"
    toText OperatorTypeRefEq = "REF_EQ"

instance ToByteString OperatorType

instance FromJSON OperatorType

instance ToJSON OperatorType

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.
data TaskStatus
    = TaskStatusFailed -- ^ FAILED
    | TaskStatusFalse -- ^ FALSE
    | TaskStatusFinished -- ^ FINISHED
      deriving (Eq, Show, Generic)

instance Hashable TaskStatus

instance FromText TaskStatus where
    parser = match "FAILED" TaskStatusFailed
         <|> match "FALSE" TaskStatusFalse
         <|> match "FINISHED" TaskStatusFinished

instance ToText TaskStatus where
    toText TaskStatusFailed = "FAILED"
    toText TaskStatusFalse = "FALSE"
    toText TaskStatusFinished = "FINISHED"

instance ToByteString TaskStatus

instance ToJSON TaskStatus

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
newtype Query = Query
    { _qSelectors :: [Selector]
      -- ^ List of selectors that define the query. An object must satisfy
      -- all of the selectors to match the query.
    } deriving (Show, Generic)

instance ToJSON Query

-- | A key-value pair that describes a property of a pipeline object. The value
-- is specified as either a string value (StringValue) or a reference to
-- another object (RefValue) but not as both.
data Field = Field
    { _fRefValue :: Maybe Text
      -- ^ The field value, expressed as the identifier of another object.
    , _fStringValue :: Maybe Text
      -- ^ The field value, expressed as a String.
    , _fKey :: Text
      -- ^ The field identifier.
    } deriving (Show, Generic)

instance FromJSON Field

instance ToJSON Field

-- | Identity information for the Amazon EC2 instance that is hosting the task
-- runner. You can get this value by calling the URI,
-- http://169.254.169.254/latest/meta-data/instance-id, from the EC2 instance.
-- For more information, go to Instance Metadata in the Amazon Elastic Compute
-- Cloud User Guide. Passing in this value proves that your task runner is
-- running on an EC2 instance, and ensures the proper AWS Data Pipeline
-- service charges are applied to your pipeline.
data InstanceIdentity = InstanceIdentity
    { _ikSignature :: Maybe Text
      -- ^ A signature which can be used to verify the accuracy and
      -- authenticity of the information provided in the instance identity
      -- document.
    , _ikDocument :: Maybe Text
      -- ^ A description of an Amazon EC2 instance that is generated when
      -- the instance is launched and exposed to the instance via the
      -- instance metadata service in the form of a JSON representation of
      -- an object.
    } deriving (Show, Generic)

instance ToJSON InstanceIdentity

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
data Operator = Operator
    { _oValues :: [Text]
      -- ^ The value that the actual field value will be compared with.
    , _oType :: Maybe OperatorType
      -- ^ The logical operation to be performed: equal (EQ), equal
      -- reference (REF_EQ), less than or equal (LE), greater than or
      -- equal (GE), or between (BETWEEN). Equal reference (REF_EQ) can be
      -- used only with reference fields. The other comparison types can
      -- be used only with String fields. The comparison types you can use
      -- apply only to certain object fields, as detailed below. The
      -- comparison operators EQ and REF_EQ act on the following fields:
      -- name @sphere parent @componentParent @instanceParent @status
      -- @scheduledStartTime @scheduledEndTime @actualStartTime
      -- @actualEndTime The comparison operators GE, LE, and BETWEEN act
      -- on the following fields: @scheduledStartTime @scheduledEndTime
      -- @actualStartTime @actualEndTime Note that fields beginning with
      -- the at sign (@) are read-only and set by the web service. When
      -- you name fields, you should choose names containing only
      -- alpha-numeric values, as symbols may be reserved by AWS Data
      -- Pipeline. User-defined fields that you add to a pipeline should
      -- prefix their name with the string "my".
    } deriving (Show, Generic)

instance FromJSON Operator

instance ToJSON Operator

-- | Contains pipeline metadata.
data PipelineDescription = PipelineDescription
    { _pdPipelineId :: Text
      -- ^ The pipeline identifier that was assigned by AWS Data Pipeline.
      -- This is a string of the form df-297EG78HU43EEXAMPLE.
    , _pdName :: Text
      -- ^ Name of the pipeline.
    , _pdDescription :: Maybe Text
      -- ^ Description of the pipeline.
    , _pdFields :: [Field]
      -- ^ A list of read-only fields that contain metadata about the
      -- pipeline: @userId, @accountId, and @pipelineState.
    } deriving (Show, Generic)

instance FromJSON PipelineDescription

-- | Contains the name and identifier of a pipeline.
data PipelineIdName = PipelineIdName
    { _pinName :: Maybe Text
      -- ^ Name of the pipeline.
    , _pinId :: Maybe Text
      -- ^ Identifier of the pipeline that was assigned by AWS Data
      -- Pipeline. This is a string of the form df-297EG78HU43EEXAMPLE.
    } deriving (Show, Generic)

instance FromJSON PipelineIdName

-- | Contains information about a pipeline object. This can be a logical,
-- physical, or physical attempt pipeline object. The complete set of
-- components of a pipeline defines the pipeline.
data PipelineObject = PipelineObject
    { _poName :: Text
      -- ^ Name of the object.
    , _poId :: Text
      -- ^ Identifier of the object.
    , _poFields :: [Field]
      -- ^ Key-value pairs that define the properties of the object.
    } deriving (Show, Generic)

instance FromJSON PipelineObject

instance ToJSON PipelineObject

-- | A comparision that is used to determine whether a query should return this
-- object.
data Selector = Selector
    { _uOperator :: Maybe Operator
      -- ^ Contains a logical operation for comparing the value of a field
      -- with a specified value.
    , _uFieldName :: Maybe Text
      -- ^ The name of the field that the operator will be applied to. The
      -- field name is the "key" portion of the field definition in the
      -- pipeline definition syntax that is used by the AWS Data Pipeline
      -- API. If the field is not set on the object, the condition fails.
    } deriving (Show, Generic)

instance ToJSON Selector

-- | An instance of PollForTaskResult, which contains an instance of TaskObject.
-- The returned object contains all the information needed to complete the
-- task that is being assigned to the task runner. One of the fields returned
-- in this object is taskId, which contains an identifier for the task being
-- assigned. The calling task runner uses taskId in subsequent calls to
-- ReportTaskProgress and SetTaskStatus.
data TaskObject = TaskObject
    { _toPipelineId :: Maybe Text
      -- ^ Identifier of the pipeline that provided the task.
    , _toAttemptId :: Maybe Text
      -- ^ Identifier of the pipeline task attempt object. AWS Data Pipeline
      -- uses this value to track how many times a task is attempted.
    , _toTaskId :: Maybe Text
      -- ^ An internal identifier for the task. This ID is passed to the
      -- SetTaskStatus and ReportTaskProgress actions.
    , _toObjects :: HashMap Text PipelineObject
      -- ^ Connection information for the location where the task runner
      -- will publish the output of the task.
    } deriving (Show, Generic)

instance FromJSON TaskObject

-- | Defines a validation error returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation errors prevent pipeline activation.
-- The set of validation errors that can be returned are defined by AWS Data
-- Pipeline.
data ValidationError = ValidationError
    { _vfId :: Maybe Text
      -- ^ The identifier of the object that contains the validation error.
    , _vfErrors :: [Text]
      -- ^ A description of the validation error.
    } deriving (Show, Generic)

instance FromJSON ValidationError

-- | Defines a validation warning returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation warnings do not prevent pipeline
-- activation. The set of validation warnings that can be returned are defined
-- by AWS Data Pipeline.
data ValidationWarning = ValidationWarning
    { _vxWarnings :: [Text]
      -- ^ A description of the validation warning.
    , _vxId :: Maybe Text
      -- ^ The identifier of the object that contains the validation
      -- warning.
    } deriving (Show, Generic)

instance FromJSON ValidationWarning

-- Newtypes
makeIso ''Query

-- Products
makeLenses ''Field
makeLenses ''InstanceIdentity
makeLenses ''Operator
makeLenses ''PipelineDescription
makeLenses ''PipelineIdName
makeLenses ''PipelineObject
makeLenses ''Selector
makeLenses ''TaskObject
makeLenses ''ValidationError
makeLenses ''ValidationWarning