{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SWF.Types where

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

import Network.AWS.SWF.Service

-- | Contains information about a workflow type.
data WorkflowTypeInfo = WorkflowTypeInfo
    { wtiCreationDate :: !UTCTime
      -- ^ The date when this type was registered.
    , wtiDeprecationDate :: Maybe UTCTime
      -- ^ If the type is in deprecated state, then it is set to the date when the
      -- type was deprecated.
    , wtiDescription :: Maybe Text
      -- ^ The description of the type registered through RegisterWorkflowType.
    , wtiStatus :: !RegistrationStatus
      -- ^ The current status of the workflow type.
    , wtiWorkflowType :: WorkflowType
      -- ^ The workflow type this information is about.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowTypeInfo
instance ToJSON WorkflowTypeInfo

-- | If specified, only executions of the type specified in the filter are
-- returned. executionFilter, typeFilter and tagFilter are mutually exclusive.
-- You can specify at most one of these in a request.
data WorkflowTypeFilter = WorkflowTypeFilter
    { wtfName :: !Text
      -- ^ Name of the workflow type. This field is required.
    , wtfVersion :: Maybe Text
      -- ^ Version of the workflow type.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowTypeFilter
instance ToJSON WorkflowTypeFilter

-- | Configuration settings of the workflow type registered through
-- RegisterWorkflowType.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration
    { wtcDefaultChildPolicy :: Maybe ChildPolicy
      -- ^ The optional default policy to use for the child workflow executions when a
      -- workflow execution of this type is terminated, by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired timeout.
      -- This default can be overridden when starting a workflow execution using the
      -- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
      -- The supported child policies are: TERMINATE: the child executions will be
      -- terminated. REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested event in
      -- its history. It is up to the decider to take appropriate actions when it
      -- receives an execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , wtcDefaultExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering the
      -- workflow type, for executions of this workflow type. This default can be
      -- overridden when starting a workflow execution using the
      -- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , wtcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list, specified when registering the workflow
      -- type, for decisions tasks scheduled for workflow executions of this type.
      -- This default can be overridden when starting a workflow execution using the
      -- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
    , wtcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering the
      -- workflow type, that a decision task for executions of this workflow type
      -- might take before returning completion or failure. If the task does not
      -- close in the specified time then the task is automatically timed out and
      -- rescheduled. If the decider eventually reports a completion or failure, it
      -- is ignored. This default can be overridden when starting a workflow
      -- execution using the StartWorkflowExecution action or the
      -- StartChildWorkflowExecution Decision. The valid values are integers greater
      -- than or equal to 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowTypeConfiguration
instance ToJSON WorkflowTypeConfiguration

-- | The type of the workflow execution.
data WorkflowType = WorkflowType
    { wtName :: !Text
      -- ^ The name of the workflow type. This field is required. The combination of
      -- workflow type name and version must be unique with in a domain.
    , wtVersion :: !Text
      -- ^ The version of the workflow type. This field is required. The combination
      -- of workflow type name and version must be unique with in a domain.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowType
instance ToJSON WorkflowType

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes
    { wetoeaChildPolicy :: !ChildPolicy
      -- ^ The policy used for the child workflow executions of this workflow
      -- execution. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
      -- attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , wetoeaTimeoutType :: !WorkflowExecutionTimeoutType
      -- ^ The type of timeout that caused this event.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionTimedOutEventAttributes
instance ToJSON WorkflowExecutionTimedOutEventAttributes

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes
    { weteaCause :: Maybe WorkflowExecutionTerminatedCause
      -- ^ If set, indicates that the workflow execution was automatically terminated,
      -- and specifies the cause. This happens if the parent workflow execution
      -- times out or is terminated and the child policy is set to terminate child
      -- executions.
    , weteaChildPolicy :: !ChildPolicy
      -- ^ The policy used for the child workflow executions of this workflow
      -- execution. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
      -- attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , weteaDetails :: Maybe Text
      -- ^ The details provided for the termination (if any).
    , weteaReason :: Maybe Text
      -- ^ The reason provided for the termination (if any).
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionTerminatedEventAttributes
instance ToJSON WorkflowExecutionTerminatedEventAttributes

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes
    { wesebChildPolicy :: !ChildPolicy
      -- ^ The policy to use for the child workflow executions if this workflow
      -- execution is terminated, by calling the TerminateWorkflowExecution action
      -- explicitly or due to an expired timeout. The supported child policies are:
      -- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , wesebContinuedExecutionRunId :: Maybe Text
      -- ^ If this workflow execution was started due to a
      -- ContinueAsNewWorkflowExecution decision, then it contains the runId of the
      -- previous workflow execution that was closed and continued as this
      -- execution.
    , wesebExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this workflow execution. The valid values are
      -- integers greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , wesebInput :: Maybe Text
      -- ^ The input provided to the workflow execution (if any).
    , wesebParentInitiatedEventId :: Maybe Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this workflow execution.
      -- The source event with this Id can be found in the history of the source
      -- workflow execution. This information can be useful for diagnosing problems
      -- by tracing back the chain of events leading up to this event.
    , wesebParentWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The source workflow execution that started this workflow execution. The
      -- member is not set if the workflow execution was not started by a workflow.
    , wesebTagList :: [Text]
      -- ^ The list of tags associated with this workflow execution. An execution can
      -- have up to 5 tags.
    , wesebTaskList :: TaskList
      -- ^ The name of the task list for scheduling the decision tasks for this
      -- workflow execution.
    , wesebTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for this workflow type. The valid
      -- values are integers greater than or equal to 0. An integer value can be
      -- used to specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , wesebWorkflowType :: WorkflowType
      -- ^ The workflow type of this execution.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionStartedEventAttributes
instance ToJSON WorkflowExecutionStartedEventAttributes

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes
    { weseaExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event corresponding
      -- to the SignalExternalWorkflow decision to signal this workflow
      -- execution.The source event with this Id can be found in the history of the
      -- source workflow execution. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event. This
      -- field is set only if the signal was initiated by another workflow
      -- execution.
    , weseaExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The workflow execution that sent the signal. This is set only of the signal
      -- was sent by another workflow execution.
    , weseaInput :: Maybe Text
      -- ^ Inputs provided with the signal (if any). The decider can use the signal
      -- name and inputs to determine how to process the signal.
    , weseaSignalName :: !Text
      -- ^ The name of the signal received. The decider can use the signal name and
      -- inputs to determine how to the process the signal.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionSignaledEventAttributes
instance ToJSON WorkflowExecutionSignaledEventAttributes

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts
    { weocOpenActivityTasks :: !Int
      -- ^ The count of activity tasks whose status is OPEN.
    , weocOpenChildWorkflowExecutions :: !Int
      -- ^ The count of child workflow executions whose status is OPEN.
    , weocOpenDecisionTasks :: !Int
      -- ^ The count of decision tasks whose status is OPEN. A workflow execution can
      -- have at most one open decision task.
    , weocOpenTimers :: !Int
      -- ^ The count of timers started by this workflow execution that have not fired
      -- yet.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionOpenCounts
instance ToJSON WorkflowExecutionOpenCounts

-- | Contains information about a workflow execution. DescribeWorkflowExecution.
-- -->.
data WorkflowExecutionInfo = WorkflowExecutionInfo
    { weiCancelRequested :: Maybe Bool
      -- ^ Set to true if a cancellation is requested for this workflow execution.
    , weiCloseStatus :: Maybe CloseStatus
      -- ^ If the execution status is closed then this specifies how the execution was
      -- closed: COMPLETED: the execution was successfully completed. CANCELED: the
      -- execution was canceled.Cancellation allows the implementation to gracefully
      -- clean up before the execution is closed. TERMINATED: the execution was
      -- force terminated. FAILED: the execution failed to complete. TIMED_OUT: the
      -- execution did not complete in the alloted time and was automatically timed
      -- out. CONTINUED_AS_NEW: the execution is logically continued. This means the
      -- current execution was completed and a new execution was started to carry on
      -- the workflow.
    , weiCloseTimestamp :: Maybe UTCTime
      -- ^ The time when the workflow execution was closed. Set only if the execution
      -- status is CLOSED.
    , weiExecution :: WorkflowExecution
      -- ^ The workflow execution this information is about.
    , weiExecutionStatus :: !ExecutionStatus
      -- ^ The current status of the execution.
    , weiParent :: Maybe WorkflowExecution
      -- ^ If this workflow execution is a child of another execution then contains
      -- the workflow execution that started this execution.
    , weiStartTimestamp :: !UTCTime
      -- ^ The time when the execution was started.
    , weiTagList :: [Text]
      -- ^ The list of tags associated with the workflow execution. Tags can be used
      -- to identify and list workflow executions of interest through the visibility
      -- APIs. A workflow execution can have a maximum of 5 tags.
    , weiWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionInfo
instance ToJSON WorkflowExecutionInfo

-- | If specified, only workflow executions matching the workflow id specified
-- in the filter are returned. executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter
    { wefWorkflowId :: Text
      -- ^ The workflowId to pass of match the criteria of this filter.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionFilter
instance ToJSON WorkflowExecutionFilter

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes
    { wefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the FailWorkflowExecution decision to fail this
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the cause of events.
    , wefeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , wefeaReason :: Maybe Text
      -- ^ The descriptive reason provided for the failure (if any).
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionFailedEventAttributes
instance ToJSON WorkflowExecutionFailedEventAttributes

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes
    { wecaneaChildPolicy :: !ChildPolicy
      -- ^ The policy to use for the child workflow executions of the new execution if
      -- it is terminated by calling the TerminateWorkflowExecution action
      -- explicitly or due to an expired timeout. The supported child policies are:
      -- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , wecaneaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the ContinueAsNewWorkflowExecution decision that
      -- started this execution. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    , wecaneaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration allowed for the new workflow execution. The valid values
      -- are integers greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , wecaneaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , wecaneaNewExecutionRunId :: !Text
      -- ^ The runId of the new workflow execution.
    , wecaneaTagList :: [Text]
      -- ^ The list of tags associated with the new workflow execution.
    , wecaneaTaskList :: TaskList
      -- ^ Represents a task list.
    , wecaneaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for the new workflow execution. The
      -- valid values are integers greater than or equal to 0. An integer value can
      -- be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , wecaneaWorkflowType :: WorkflowType
      -- ^ Represents a workflow type.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionContinuedAsNewEventAttributes
instance ToJSON WorkflowExecutionContinuedAsNewEventAttributes

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration
    { wecChildPolicy :: !ChildPolicy
      -- ^ The policy to use for the child workflow executions if this workflow
      -- execution is terminated, by calling the TerminateWorkflowExecution action
      -- explicitly or due to an expired timeout. The supported child policies are:
      -- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , wecExecutionStartToCloseTimeout :: !Text
      -- ^ The total duration for this workflow execution. The valid values are
      -- integers greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , wecTaskList :: TaskList
      -- ^ The task list used for the decision tasks generated for this workflow
      -- execution.
    , wecTaskStartToCloseTimeout :: !Text
      -- ^ The maximum duration allowed for decision tasks for this workflow
      -- execution. The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds while NONE can
      -- be used to specify unlimited duration.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionConfiguration
instance ToJSON WorkflowExecutionConfiguration

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes
    { wecebDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CompleteWorkflowExecution decision to complete
      -- this execution. This information can be useful for diagnosing problems by
      -- tracing back the cause of events.
    , wecebResult :: Maybe Text
      -- ^ The result produced by the workflow execution upon successful completion.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionCompletedEventAttributes
instance ToJSON WorkflowExecutionCompletedEventAttributes

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes
    { weceaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CancelWorkflowExecution decision for this
      -- cancellation request. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    , weceaDetails :: Maybe Text
      -- ^ Details for the cancellation (if any).
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionCanceledEventAttributes
instance ToJSON WorkflowExecutionCanceledEventAttributes

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { wecreaCause :: Maybe WorkflowExecutionCancelRequestedCause
      -- ^ If set, indicates that the request to cancel the workflow execution was
      -- automatically generated, and specifies the cause. This happens if the
      -- parent workflow execution times out or is terminated, and the child policy
      -- is set to cancel child executions.
    , wecreaExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated event
      -- corresponding to the RequestCancelExternalWorkflowExecution decision to
      -- cancel this workflow execution.The source event with this Id can be found
      -- in the history of the source workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , wecreaExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The external workflow execution for which the cancellation was requested.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecutionCancelRequestedEventAttributes
instance ToJSON WorkflowExecutionCancelRequestedEventAttributes

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
data WorkflowExecution = WorkflowExecution
    { weRunId :: !Text
      -- ^ A system generated unique identifier for the workflow execution.
    , weWorkflowId :: !Text
      -- ^ The user defined identifier associated with the workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON WorkflowExecution
instance ToJSON WorkflowExecution

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerStartedEventAttributes = TimerStartedEventAttributes
    { tseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks.
    , tseaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the StartTimer decision for this activity task. This
      -- information can be useful for diagnosing problems by tracing back the cause
      -- of events.
    , tseaStartToFireTimeout :: !Text
      -- ^ The duration of time after which the timer will fire. The duration is
      -- specified in seconds. The valid values are integers greater than or equal
      -- to 0.
    , tseaTimerId :: !Text
      -- ^ The unique Id of the timer that was started.
    } deriving (Eq, Show, Generic)

instance FromJSON TimerStartedEventAttributes
instance ToJSON TimerStartedEventAttributes

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerFiredEventAttributes = TimerFiredEventAttributes
    { tfeaStartedEventId :: !Integer
      -- ^ The id of the TimerStarted event that was recorded when this timer was
      -- started. This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    , tfeaTimerId :: !Text
      -- ^ The unique Id of the timer that fired.
    } deriving (Eq, Show, Generic)

instance FromJSON TimerFiredEventAttributes
instance ToJSON TimerFiredEventAttributes

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes
    { tceaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CancelTimer decision to cancel this timer. This
      -- information can be useful for diagnosing problems by tracing back the cause
      -- of events.
    , tceaStartedEventId :: !Integer
      -- ^ The id of the TimerStarted event that was recorded when this timer was
      -- started. This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    , tceaTimerId :: !Text
      -- ^ The unique Id of the timer that was canceled.
    } deriving (Eq, Show, Generic)

instance FromJSON TimerCanceledEventAttributes
instance ToJSON TimerCanceledEventAttributes

-- | If set, specifies the default task list to use for scheduling tasks of this
-- activity type. This default task list is used if a task list is not
-- provided when a task is scheduled through the ScheduleActivityTask
-- Decision.
newtype TaskList = TaskList
    { tlName :: Text
      -- ^ The name of the task list.
    } deriving (Eq, Show, Generic)

instance FromJSON TaskList
instance ToJSON TaskList

-- | If specified, only executions that have the matching tag are listed.
-- executionFilter, typeFilter and tagFilter are mutually exclusive. You can
-- specify at most one of these in a request.
newtype TagFilter = TagFilter
    { tfTag :: Text
      -- ^ Specifies the tag that must be associated with the execution for it to meet
      -- the filter criteria. This field is required.
    } deriving (Eq, Show, Generic)

instance FromJSON TagFilter
instance ToJSON TagFilter

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes
    { stfeaCause :: !StartTimerFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , stfeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the StartTimer decision for this activity task. This
      -- information can be useful for diagnosing problems by tracing back the cause
      -- of events.
    , stfeaTimerId :: !Text
      -- ^ The timerId provided in the StartTimer decision that failed.
    } deriving (Eq, Show, Generic)

instance FromJSON StartTimerFailedEventAttributes
instance ToJSON StartTimerFailedEventAttributes

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes
    { stdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks.
    , stdaStartToFireTimeout :: !Text
      -- ^ The duration to wait before firing the timer. This field is required. The
      -- duration is specified in seconds. The valid values are integers greater
      -- than or equal to 0.
    , stdaTimerId :: !Text
      -- ^ The unique Id of the timer. This field is required. The specified string
      -- must not start or end with whitespace. It must not contain a : (colon), /
      -- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
      -- \u007f - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    } deriving (Eq, Show, Generic)

instance FromJSON StartTimerDecisionAttributes
instance ToJSON StartTimerDecisionAttributes

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes
    { scweieaChildPolicy :: !ChildPolicy
      -- ^ The policy to use for the child workflow executions if this execution gets
      -- terminated by explicitly calling the TerminateWorkflowExecution action or
      -- due to an expired timeout. The supported child policies are: TERMINATE: the
      -- child executions will be terminated. REQUEST_CANCEL: a request to cancel
      -- will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run.
    , scweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent decision tasks. This data is not sent to the activity.
    , scweieaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the StartChildWorkflowExecution Decision to request
      -- this child workflow execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , scweieaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for the child workflow execution. If the workflow
      -- execution is not closed within this duration, it will be timed out and
      -- force terminated. The valid values are integers greater than or equal to 0.
      -- An integer value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , scweieaInput :: Maybe Text
      -- ^ The inputs provided to the child workflow execution (if any).
    , scweieaTagList :: [Text]
      -- ^ The list of tags to associated with the child workflow execution.
    , scweieaTaskList :: TaskList
      -- ^ The name of the task list used for the decision tasks of the child workflow
      -- execution.
    , scweieaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration allowed for the decision tasks for this workflow
      -- execution. The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds while NONE can
      -- be used to specify unlimited duration.
    , scweieaWorkflowId :: !Text
      -- ^ The workflowId of the child workflow execution.
    , scweieaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON StartChildWorkflowExecutionInitiatedEventAttributes
instance ToJSON StartChildWorkflowExecutionInitiatedEventAttributes

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes
    { scwefeaCause :: !StartChildWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , scwefeaControl :: Maybe Text
    , scwefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the StartChildWorkflowExecution Decision to request
      -- this child workflow execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , scwefeaInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , scwefeaWorkflowId :: !Text
      -- ^ The workflowId of the child workflow execution.
    , scwefeaWorkflowType :: WorkflowType
      -- ^ The workflow type provided in the StartChildWorkflowExecution Decision that
      -- failed.
    } deriving (Eq, Show, Generic)

instance FromJSON StartChildWorkflowExecutionFailedEventAttributes
instance ToJSON StartChildWorkflowExecutionFailedEventAttributes

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes
    { scwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow executions if
      -- the workflow execution being started is terminated by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired timeout.
      -- This policy overrides the default child policy specified when registering
      -- the workflow type using RegisterWorkflowType. The supported child policies
      -- are: TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run. A child policy for the workflow execution being
      -- started must be specified either as a default registered for its workflow
      -- type or through this field. If neither this field is set nor a default
      -- child policy was specified at registration time then a fault will be
      -- returned.
    , scwedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks. This data is not sent to the child workflow
      -- execution.
    , scwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration for this workflow execution. This overrides the
      -- defaultExecutionStartToCloseTimeout specified when registering the workflow
      -- type. The valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE can be used
      -- to specify unlimited duration. An execution start-to-close timeout for this
      -- workflow execution must be specified either as a default for the workflow
      -- type or through this parameter. If neither this parameter is set nor a
      -- default execution start-to-close timeout was specified at registration time
      -- then a fault will be returned.
    , scwedaInput :: Maybe Text
      -- ^ The input to be provided to the workflow execution.
    , scwedaTagList :: [Text]
      -- ^ The list of tags to associate with the child workflow execution. A maximum
      -- of 5 tags can be specified. You can list workflow executions with a
      -- specific tag by calling ListOpenWorkflowExecutions or
      -- ListClosedWorkflowExecutions and specifying a TagFilter.
    , scwedaTaskList :: Maybe TaskList
      -- ^ The name of the task list to be used for decision tasks of the child
      -- workflow execution. A task list for this workflow execution must be
      -- specified either as a default for the workflow type or through this
      -- parameter. If neither this parameter is set nor a default task list was
      -- specified at registration time then a fault will be returned. The specified
      -- string must not start or end with whitespace. It must not contain a :
      -- (colon), / (slash), | (vertical bar), or any control characters
      -- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
      -- string &quot;arn&quot;.
    , scwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for this workflow
      -- execution. This parameter overrides the defaultTaskStartToCloseTimout
      -- specified when registering the workflow type using RegisterWorkflowType.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for this workflow
      -- execution must be specified either as a default for the workflow type or
      -- through this parameter. If neither this parameter is set nor a default task
      -- start-to-close timeout was specified at registration time then a fault will
      -- be returned.
    , scwedaWorkflowId :: !Text
      -- ^ The workflowId of the workflow execution. This field is required. The
      -- specified string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control characters
      -- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
      -- string &quot;arn&quot;.
    , scwedaWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution to be started. This field is required.
    } deriving (Eq, Show, Generic)

instance FromJSON StartChildWorkflowExecutionDecisionAttributes
instance ToJSON StartChildWorkflowExecutionDecisionAttributes

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { seweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent decision tasks.
    , seweieaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the SignalExternalWorkflowExecution decision for this
      -- signal. This information can be useful for diagnosing problems by tracing
      -- back the cause of events leading up to this event.
    , seweieaInput :: Maybe Text
      -- ^ Input provided to the signal (if any).
    , seweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to send the signal to.
    , seweieaSignalName :: !Text
      -- ^ The name of the signal.
    , seweieaWorkflowId :: !Text
      -- ^ The workflowId of the external workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionInitiatedEventAttributes
instance ToJSON SignalExternalWorkflowExecutionInitiatedEventAttributes

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes
    { sewefeaCause :: !SignalExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , sewefeaControl :: Maybe Text
    , sewefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the SignalExternalWorkflowExecution decision for this
      -- signal. This information can be useful for diagnosing problems by tracing
      -- back the cause of events leading up to this event.
    , sewefeaInitiatedEventId :: !Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event corresponding
      -- to the SignalExternalWorkflowExecution decision to request this signal.
      -- This information can be useful for diagnosing problems by tracing back the
      -- chain of events leading up to this event.
    , sewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution that the signal was being
      -- delivered to.
    , sewefeaWorkflowId :: !Text
      -- ^ The workflowId of the external workflow execution that the signal was being
      -- delivered to.
    } deriving (Eq, Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionFailedEventAttributes
instance ToJSON SignalExternalWorkflowExecutionFailedEventAttributes

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes
    { sewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent decision tasks.
    , sewedaInput :: Maybe Text
      -- ^ Optional input to be provided with the signal.The target workflow execution
      -- will use the signal name and input to process the signal.
    , sewedaRunId :: Maybe Text
      -- ^ The runId of the workflow execution to be signaled.
    , sewedaSignalName :: !Text
      -- ^ The name of the signal.The target workflow execution will use the signal
      -- name and input to process the signal. This field is required.
    , sewedaWorkflowId :: !Text
      -- ^ The workflowId of the workflow execution to be signaled. This field is
      -- required.
    } deriving (Eq, Show, Generic)

instance FromJSON SignalExternalWorkflowExecutionDecisionAttributes
instance ToJSON SignalExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes
    { satfeaActivityId :: !Text
      -- ^ The activityId provided in the ScheduleActivityTask decision that failed.
    , satfeaActivityType :: ActivityType
      -- ^ The activity type provided in the ScheduleActivityTask decision that
      -- failed.
    , satfeaCause :: !ScheduleActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , satfeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- that resulted in the scheduling of this activity task. This information can
      -- be useful for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ScheduleActivityTaskFailedEventAttributes
instance ToJSON ScheduleActivityTaskFailedEventAttributes

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes
    { satdaActivityId :: !Text
      -- ^ The activityId of the activity task. This field is required. The specified
      -- string must not start or end with whitespace. It must not contain a :
      -- (colon), / (slash), | (vertical bar), or any control characters
      -- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
      -- string &quot;arn&quot;.
    , satdaActivityType :: ActivityType
      -- ^ The type of the activity task to schedule. This field is required.
    , satdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks. This data is not sent to the activity.
    , satdaHeartbeatTimeout :: Maybe Text
      -- ^ If set, specifies the maximum time before which a worker processing a task
      -- of this type must report progress by calling RecordActivityTaskHeartbeat.
      -- If the timeout is exceeded, the activity task is automatically timed out.
      -- If the worker subsequently attempts to record a heartbeat or returns a
      -- result, it will be ignored. This overrides the default heartbeat timeout
      -- specified when registering the activity type using RegisterActivityType.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , satdaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , satdaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this activity task. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to specify the
      -- duration in seconds while NONE can be used to specify unlimited duration. A
      -- schedule-to-close timeout for this activity task must be specified either
      -- as a default for the activity type or through this field. If neither this
      -- field is set nor a default schedule-to-close timeout was specified at
      -- registration time then a fault will be returned.
    , satdaScheduleToStartTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration the activity task can wait to be
      -- assigned to a worker. This overrides the default schedule-to-start timeout
      -- specified when registering the activity type using RegisterActivityType.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A schedule-to-start timeout for this activity
      -- task must be specified either as a default for the activity type or through
      -- this field. If neither this field is set nor a default schedule-to-start
      -- timeout was specified at registration time then a fault will be returned.
    , satdaStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration a worker may take to process this
      -- activity task. This overrides the default start-to-close timeout specified
      -- when registering the activity type using RegisterActivityType. The valid
      -- values are integers greater than or equal to 0. An integer value can be
      -- used to specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. A start-to-close timeout for this activity task must be
      -- specified either as a default for the activity type or through this field.
      -- If neither this field is set nor a default start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , satdaTaskList :: Maybe TaskList
      -- ^ If set, specifies the name of the task list in which to schedule the
      -- activity task. If not specified, the defaultTaskList registered with the
      -- activity type will be used. A task list for this activity task must be
      -- specified either as a default for the activity type or through this field.
      -- If neither this field is set nor a default task list was specified at
      -- registration time then a fault will be returned. The specified string must
      -- not start or end with whitespace. It must not contain a : (colon), /
      -- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
      -- \u007f - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    } deriving (Eq, Show, Generic)

instance FromJSON ScheduleActivityTaskDecisionAttributes
instance ToJSON ScheduleActivityTaskDecisionAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { rceweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks.
    , rceweieaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RequestCancelExternalWorkflowExecution decision
      -- for this cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , rceweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to be canceled.
    , rceweieaWorkflowId :: !Text
      -- ^ The workflowId of the external workflow execution to be canceled.
    } deriving (Eq, Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
instance ToJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { rcewefeaCause :: !RequestCancelExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , rcewefeaControl :: Maybe Text
    , rcewefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RequestCancelExternalWorkflowExecution decision
      -- for this cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , rcewefeaInitiatedEventId :: !Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated event
      -- corresponding to the RequestCancelExternalWorkflowExecution decision to
      -- cancel this external workflow execution. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , rcewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution.
    , rcewefeaWorkflowId :: !Text
      -- ^ The workflowId of the external workflow to which the cancel request was to
      -- be delivered.
    } deriving (Eq, Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes
instance ToJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { rcewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks.
    , rcewedaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to cancel.
    , rcewedaWorkflowId :: !Text
      -- ^ The workflowId of the external workflow execution to cancel. This field is
      -- required.
    } deriving (Eq, Show, Generic)

instance FromJSON RequestCancelExternalWorkflowExecutionDecisionAttributes
instance ToJSON RequestCancelExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes
    { rcatfeaActivityId :: !Text
      -- ^ The activityId provided in the RequestCancelActivityTask decision that
      -- failed.
    , rcatfeaCause :: !RequestCancelActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , rcatfeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RequestCancelActivityTask decision for this
      -- cancellation request. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON RequestCancelActivityTaskFailedEventAttributes
instance ToJSON RequestCancelActivityTaskFailedEventAttributes

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { rcatdaActivityId :: Text
      -- ^ The activityId of the activity task to be canceled.
    } deriving (Eq, Show, Generic)

instance FromJSON RequestCancelActivityTaskDecisionAttributes
instance ToJSON RequestCancelActivityTaskDecisionAttributes

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes
    { rmfeaCause :: !RecordMarkerFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , rmfeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RecordMarkerFailed decision for this cancellation
      -- request. This information can be useful for diagnosing problems by tracing
      -- back the cause of events.
    , rmfeaMarkerName :: !Text
      -- ^ The marker's name.
    } deriving (Eq, Show, Generic)

instance FromJSON RecordMarkerFailedEventAttributes
instance ToJSON RecordMarkerFailedEventAttributes

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes
    { rmdaDetails :: Maybe Text
      -- ^ Optional details of the marker.
    , rmdaMarkerName :: !Text
      -- ^ The name of the marker. This file is required.
    } deriving (Eq, Show, Generic)

instance FromJSON RecordMarkerDecisionAttributes
instance ToJSON RecordMarkerDecisionAttributes

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes
    { mreaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RecordMarker decision that requested this marker.
      -- This information can be useful for diagnosing problems by tracing back the
      -- cause of events.
    , mreaDetails :: Maybe Text
      -- ^ Details of the marker (if any).
    , mreaMarkerName :: !Text
      -- ^ The name of the marker.
    } deriving (Eq, Show, Generic)

instance FromJSON MarkerRecordedEventAttributes
instance ToJSON MarkerRecordedEventAttributes

-- | Event within a workflow execution. A history event can be one of these
-- types: WorkflowExecutionStarted: The workflow execution was started.
-- WorkflowExecutionCompleted: The workflow execution was closed due to
-- successful completion. WorkflowExecutionFailed: The workflow execution
-- closed due to a failure. WorkflowExecutionTimedOut: The workflow execution
-- was closed because a time out was exceeded. WorkflowExecutionCanceled: The
-- workflow execution was successfully canceled and closed.
-- WorkflowExecutionTerminated: The workflow execution was terminated.
-- WorkflowExecutionContinuedAsNew: The workflow execution was closed and a
-- new execution of the same type was created with the same workflowId.
-- WorkflowExecutionCancelRequested: A request to cancel this workflow
-- execution was made. DecisionTaskScheduled: A decision task was scheduled
-- for the workflow execution. DecisionTaskStarted: The decision task was
-- dispatched to a decider. DecisionTaskCompleted: The decider successfully
-- completed a decision task by calling RespondDecisionTaskCompleted.
-- DecisionTaskFailed: The decider failed a decision task by calling
-- RespondDecisionTaskFailed. --> DecisionTaskTimedOut: The decision task
-- timed out. ActivityTaskScheduled: An activity task was scheduled for
-- execution. ScheduleActivityTaskFailed: Failed to process
-- ScheduleActivityTask decision. This happens when the decision is not
-- configured properly, for example the activity type specified is not
-- registered. ActivityTaskStarted: The scheduled activity task was dispatched
-- to a worker. ActivityTaskCompleted: An activity worker successfully
-- completed an activity task by calling RespondActivityTaskCompleted.
-- ActivityTaskFailed: An activity worker failed an activity task by calling
-- RespondActivityTaskFailed. ActivityTaskTimedOut: The activity task timed
-- out. ActivityTaskCanceled: The activity task was successfully canceled.
-- ActivityTaskHeartbeatRecorded: A call to RecordActivityTaskHeartbeat was
-- successfully processed by the system. --> ActivityTaskCancelRequested: A
-- RequestCancelActivityTask decision was received by the system.
-- RequestCancelActivityTaskFailed: Failed to process
-- RequestCancelActivityTask decision. This happens when the decision is not
-- configured properly. WorkflowExecutionSignaled: An external signal was
-- received for the workflow execution. MarkerRecorded: A marker was recorded
-- in the workflow history as the result of a RecordMarker decision.
-- TimerStarted: A timer was started for the workflow execution due to a
-- StartTimer decision. StartTimerFailed: Failed to process StartTimer
-- decision. This happens when the decision is not configured properly, for
-- example a timer already exists with the specified timer Id. TimerFired: A
-- timer, previously started for this workflow execution, fired.
-- TimerCanceled: A timer, previously started for this workflow execution, was
-- successfully canceled. CancelTimerFailed: Failed to process CancelTimer
-- decision. This happens when the decision is not configured properly, for
-- example no timer exists with the specified timer Id.
-- StartChildWorkflowExecutionInitiated: A request was made to start a child
-- workflow execution. StartChildWorkflowExecutionFailed: Failed to process
-- StartChildWorkflowExecution decision. This happens when the decision is not
-- configured properly, for example the workflow type specified is not
-- registered. ChildWorkflowExecutionStarted: A child workflow execution was
-- successfully started. ChildWorkflowExecutionCompleted: A child workflow
-- execution, started by this workflow execution, completed successfully and
-- was closed. ChildWorkflowExecutionFailed: A child workflow execution,
-- started by this workflow execution, failed to complete successfully and was
-- closed. ChildWorkflowExecutionTimedOut: A child workflow execution, started
-- by this workflow execution, timed out and was closed.
-- ChildWorkflowExecutionCanceled: A child workflow execution, started by this
-- workflow execution, was canceled and closed.
-- ChildWorkflowExecutionTerminated: A child workflow execution, started by
-- this workflow execution, was terminated.
-- SignalExternalWorkflowExecutionInitiated: A request to signal an external
-- workflow was made. ExternalWorkflowExecutionSignaled: A signal, requested
-- by this workflow execution, was successfully delivered to the target
-- external workflow execution. SignalExternalWorkflowExecutionFailed: The
-- request to signal an external workflow execution failed.
-- RequestCancelExternalWorkflowExecutionInitiated: A request was made to
-- request the cancellation of an external workflow execution.
-- ExternalWorkflowExecutionCancelRequested: Request to cancel an external
-- workflow execution was successfully delivered to the target execution.
-- RequestCancelExternalWorkflowExecutionFailed: Request to cancel an external
-- workflow execution failed.
data HistoryEvent = HistoryEvent
    { heActivityTaskCancelRequestedEventAttributes :: Maybe ActivityTaskCancelRequestedEventAttributes
      -- ^ If the event is of type ActivityTaskcancelRequested then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskCanceledEventAttributes :: Maybe ActivityTaskCanceledEventAttributes
      -- ^ If the event is of type ActivityTaskCanceled then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskCompletedEventAttributes :: Maybe ActivityTaskCompletedEventAttributes
      -- ^ If the event is of type ActivityTaskCompleted then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskFailedEventAttributes :: Maybe ActivityTaskFailedEventAttributes
      -- ^ If the event is of type ActivityTaskFailed then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskScheduledEventAttributes :: Maybe ActivityTaskScheduledEventAttributes
      -- ^ If the event is of type ActivityTaskScheduled then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskStartedEventAttributes :: Maybe ActivityTaskStartedEventAttributes
      -- ^ If the event is of type ActivityTaskStarted then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heActivityTaskTimedOutEventAttributes :: Maybe ActivityTaskTimedOutEventAttributes
      -- ^ If the event is of type ActivityTaskTimedOut then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heCancelTimerFailedEventAttributes :: Maybe CancelTimerFailedEventAttributes
      -- ^ If the event is of type CancelTimerFailed then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heCancelWorkflowExecutionFailedEventAttributes :: Maybe CancelWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CancelWorkflowExecutionFailed then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionCanceledEventAttributes :: Maybe ChildWorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCanceled then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionCompletedEventAttributes :: Maybe ChildWorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCompleted then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionFailedEventAttributes :: Maybe ChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionFailed then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionStartedEventAttributes :: Maybe ChildWorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionStarted then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionTerminatedEventAttributes :: Maybe ChildWorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTerminated then this member
      -- is set and provides detailed information about the event. It is not set for
      -- other event types.
    , heChildWorkflowExecutionTimedOutEventAttributes :: Maybe ChildWorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTimedOut then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heCompleteWorkflowExecutionFailedEventAttributes :: Maybe CompleteWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CompleteWorkflowExecutionFailed then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heContinueAsNewWorkflowExecutionFailedEventAttributes :: Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ContinueAsNewWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event. It is not
      -- set for other event types.
    , heDecisionTaskCompletedEventAttributes :: Maybe DecisionTaskCompletedEventAttributes
      -- ^ If the event is of type DecisionTaskCompleted then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heDecisionTaskScheduledEventAttributes :: Maybe DecisionTaskScheduledEventAttributes
      -- ^ If the event is of type DecisionTaskScheduled then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heDecisionTaskStartedEventAttributes :: Maybe DecisionTaskStartedEventAttributes
      -- ^ If the event is of type DecisionTaskStarted then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heDecisionTaskTimedOutEventAttributes :: Maybe DecisionTaskTimedOutEventAttributes
      -- ^ If the event is of type DecisionTaskTimedOut then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heEventId :: !Integer
      -- ^ The system generated id of the event. This id uniquely identifies the event
      -- with in the workflow execution history.
    , heEventTimestamp :: !UTCTime
      -- ^ The date and time when the event occurred.
    , heEventType :: !EventType
      -- ^ The type of the history event.
    , heExternalWorkflowExecutionCancelRequestedEventAttributes :: Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionCancelRequested then this
      -- member is set and provides detailed information about the event. It is not
      -- set for other event types.
    , heExternalWorkflowExecutionSignaledEventAttributes :: Maybe ExternalWorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionSignaled then this member
      -- is set and provides detailed information about the event. It is not set for
      -- other event types.
    , heFailWorkflowExecutionFailedEventAttributes :: Maybe FailWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type FailWorkflowExecutionFailed then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heMarkerRecordedEventAttributes :: Maybe MarkerRecordedEventAttributes
      -- ^ If the event is of type MarkerRecorded then this member is set and provides
      -- detailed information about the event. It is not set for other event types.
    , heRecordMarkerFailedEventAttributes :: Maybe RecordMarkerFailedEventAttributes
      -- ^ If the event is of type DecisionTaskFailed then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heRequestCancelActivityTaskFailedEventAttributes :: Maybe RequestCancelActivityTaskFailedEventAttributes
      -- ^ If the event is of type RequestCancelActivityTaskFailed then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type RequestCancelExternalWorkflowExecutionFailed then
      -- this member is set and provides detailed information about the event. It is
      -- not set for other event types.
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type RequestCancelExternalWorkflowExecutionInitiated
      -- then this member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , heScheduleActivityTaskFailedEventAttributes :: Maybe ScheduleActivityTaskFailedEventAttributes
      -- ^ If the event is of type ScheduleActivityTaskFailed then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heSignalExternalWorkflowExecutionFailedEventAttributes :: Maybe SignalExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event. It is not
      -- set for other event types.
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionInitiated then this
      -- member is set and provides detailed information about the event. It is not
      -- set for other event types.
    , heStartChildWorkflowExecutionFailedEventAttributes :: Maybe StartChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionFailed then this member
      -- is set and provides detailed information about the event. It is not set for
      -- other event types.
    , heStartChildWorkflowExecutionInitiatedEventAttributes :: Maybe StartChildWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionInitiated then this
      -- member is set and provides detailed information about the event. It is not
      -- set for other event types.
    , heStartTimerFailedEventAttributes :: Maybe StartTimerFailedEventAttributes
      -- ^ If the event is of type StartTimerFailed then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heTimerCanceledEventAttributes :: Maybe TimerCanceledEventAttributes
      -- ^ If the event is of type TimerCanceled then this member is set and provides
      -- detailed information about the event. It is not set for other event types.
    , heTimerFiredEventAttributes :: Maybe TimerFiredEventAttributes
      -- ^ If the event is of type TimerFired then this member is set and provides
      -- detailed information about the event. It is not set for other event types.
    , heTimerStartedEventAttributes :: Maybe TimerStartedEventAttributes
      -- ^ If the event is of type TimerStarted then this member is set and provides
      -- detailed information about the event. It is not set for other event types.
    , heWorkflowExecutionCancelRequestedEventAttributes :: Maybe WorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCancelRequested then this member
      -- is set and provides detailed information about the event. It is not set for
      -- other event types.
    , heWorkflowExecutionCanceledEventAttributes :: Maybe WorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type WorkflowExecutionCanceled then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionCompletedEventAttributes :: Maybe WorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCompleted then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionContinuedAsNewEventAttributes :: Maybe WorkflowExecutionContinuedAsNewEventAttributes
      -- ^ If the event is of type WorkflowExecutionContinuedAsNew then this member is
      -- set and provides detailed information about the event. It is not set for
      -- other event types.
    , heWorkflowExecutionFailedEventAttributes :: Maybe WorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type WorkflowExecutionFailed then this member is set and
      -- provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionSignaledEventAttributes :: Maybe WorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type WorkflowExecutionSignaled then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionStartedEventAttributes :: Maybe WorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type WorkflowExecutionStarted then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionTerminatedEventAttributes :: Maybe WorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type WorkflowExecutionTerminated then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    , heWorkflowExecutionTimedOutEventAttributes :: Maybe WorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type WorkflowExecutionTimedOut then this member is set
      -- and provides detailed information about the event. It is not set for other
      -- event types.
    } deriving (Eq, Show, Generic)

instance FromJSON HistoryEvent
instance ToJSON HistoryEvent

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes
    { fwefeaCause :: !FailWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the system and
      -- can be useful for diagnostic purposes. If cause is set to
      -- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
      -- permissions. For details and example IAM policies, see Using IAM to Manage
      -- Access to Amazon SWF Workflows.
    , fwefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the FailWorkflowExecution decision to fail this
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON FailWorkflowExecutionFailedEventAttributes
instance ToJSON FailWorkflowExecutionFailedEventAttributes

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { fwedaDetails :: Maybe Text
      -- ^ Optional details of the failure.
    , fwedaReason :: Maybe Text
      -- ^ A descriptive reason for the failure that may help in diagnostics.
    } deriving (Eq, Show, Generic)

instance FromJSON FailWorkflowExecutionDecisionAttributes
instance ToJSON FailWorkflowExecutionDecisionAttributes

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes
    { eweseaInitiatedEventId :: !Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event corresponding
      -- to the SignalExternalWorkflowExecution decision to request this signal.
      -- This information can be useful for diagnosing problems by tracing back the
      -- chain of events leading up to this event.
    , eweseaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution that the signal was delivered to.
    } deriving (Eq, Show, Generic)

instance FromJSON ExternalWorkflowExecutionSignaledEventAttributes
instance ToJSON ExternalWorkflowExecutionSignaledEventAttributes

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { ewecreaInitiatedEventId :: !Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated event
      -- corresponding to the RequestCancelExternalWorkflowExecution decision to
      -- cancel this external workflow execution. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , ewecreaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution to which the cancellation request was
      -- delivered.
    } deriving (Eq, Show, Generic)

instance FromJSON ExternalWorkflowExecutionCancelRequestedEventAttributes
instance ToJSON ExternalWorkflowExecutionCancelRequestedEventAttributes

-- | Workflow executions are included in the returned results based on whether
-- their start times are within the range specified by this filter.
data ExecutionTimeFilter = ExecutionTimeFilter
    { etfLatestDate :: Maybe UTCTime
      -- ^ Specifies the latest start or close date and time to return.
    , etfOldestDate :: !UTCTime
      -- ^ Specifies the oldest start or close date and time to return.
    } deriving (Eq, Show, Generic)

instance FromJSON ExecutionTimeFilter
instance ToJSON ExecutionTimeFilter

-- | Contains general information about a domain.
data DomainInfo = DomainInfo
    { diDescription :: Maybe Text
      -- ^ The description of the domain provided through RegisterDomain.
    , diName :: !Text
      -- ^ The name of the domain. This name is unique within the account.
    , diStatus :: !RegistrationStatus
      -- ^ The status of the domain: REGISTERED: The domain is properly registered and
      -- available. You can use this domain for registering types and creating new
      -- workflow executions. DEPRECATED: The domain was deprecated using
      -- DeprecateDomain, but is still in use. You should not create new workflow
      -- executions in this domain.
    } deriving (Eq, Show, Generic)

instance FromJSON DomainInfo
instance ToJSON DomainInfo

-- | Contains the configuration settings of a domain.
newtype DomainConfiguration = DomainConfiguration
    { dcWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ The retention period for workflow executions in this domain.
    } deriving (Eq, Show, Generic)

instance FromJSON DomainConfiguration
instance ToJSON DomainConfiguration

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes
    { dttoeaScheduledEventId :: !Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when this
      -- decision task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , dttoeaStartedEventId :: !Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this decision task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , dttoeaTimeoutType :: !DecisionTaskTimeoutType
      -- ^ The type of timeout that expired before the decision task could be
      -- completed.
    } deriving (Eq, Show, Generic)

instance FromJSON DecisionTaskTimedOutEventAttributes
instance ToJSON DecisionTaskTimedOutEventAttributes

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes
    { dtsebIdentity :: Maybe Text
      -- ^ Identity of the decider making the request. This enables diagnostic tracing
      -- when problems arise. The form of this identity is user defined.
    , dtsebScheduledEventId :: !Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when this
      -- decision task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON DecisionTaskStartedEventAttributes
instance ToJSON DecisionTaskStartedEventAttributes

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes
    { dtseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this decision task. The task is considered timed
      -- out if it does not completed within this duration. The valid values are
      -- integers greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , dtseaTaskList :: TaskList
      -- ^ The name of the task list in which the decision task was scheduled.
    } deriving (Eq, Show, Generic)

instance FromJSON DecisionTaskScheduledEventAttributes
instance ToJSON DecisionTaskScheduledEventAttributes

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes
    { dtceaExecutionContext :: Maybe Text
      -- ^ User defined context for the workflow execution.
    , dtceaScheduledEventId :: !Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when this
      -- decision task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , dtceaStartedEventId :: !Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this decision task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON DecisionTaskCompletedEventAttributes
instance ToJSON DecisionTaskCompletedEventAttributes

-- | Specifies a decision made by the decider. A decision can be one of these
-- types: CancelTimer cancels a previously started timer and records a
-- TimerCanceled event in the history. CancelWorkflowExecution closes the
-- workflow execution and records a WorkflowExecutionCanceled event in the
-- history. CompleteWorkflowExecution closes the workflow execution and
-- records a WorkflowExecutionCompleted event in the history .
-- ContinueAsNewWorkflowExecution closes the workflow execution and starts a
-- new workflow execution of the same type using the same workflow id and a
-- unique run Id. A WorkflowExecutionContinuedAsNew event is recorded in the
-- history. FailWorkflowExecution closes the workflow execution and records a
-- WorkflowExecutionFailed event in the history. RecordMarker records a
-- MarkerRecorded event in the history. Markers can be used for adding custom
-- information in the history for instance to let deciders know that they do
-- not need to look at the history beyond the marker event.
-- RequestCancelActivityTask attempts to cancel a previously scheduled
-- activity task. If the activity task was scheduled but has not been assigned
-- to a worker, then it will be canceled. If the activity task was already
-- assigned to a worker, then the worker will be informed that cancellation
-- has been requested in the response to RecordActivityTaskHeartbeat.
-- RequestCancelExternalWorkflowExecution requests that a request be made to
-- cancel the specified external workflow execution and records a
-- RequestCancelExternalWorkflowExecutionInitiated event in the history.
-- ScheduleActivityTask schedules an activity task.
-- SignalExternalWorkflowExecution requests a signal to be delivered to the
-- specified external workflow execution and records a
-- SignalExternalWorkflowExecutionInitiated event in the history.
-- StartChildWorkflowExecution requests that a child workflow execution be
-- started and records a StartChildWorkflowExecutionInitiated event in the
-- history. The child workflow execution is a separate workflow execution with
-- its own history. StartTimer starts a timer for this workflow execution and
-- records a TimerStarted event in the history. This timer will fire after the
-- specified delay and record a TimerFired event. Access Control If you grant
-- permission to use RespondDecisionTaskCompleted, you can use IAM policies to
-- express permissions for the list of decisions returned by this action as if
-- they were members of the API. Treating decisions as a pseudo API maintains
-- a uniform conceptual model and helps keep policies readable. For details
-- and example IAM policies, see Using IAM to Manage Access to Amazon SWF
-- Workflows. Decision Failure Decisions can fail for several reasons The
-- ordering of decisions should follow a logical flow. Some decisions might
-- not make sense in the current context of the workflow execution and will
-- therefore fail. A limit on your account was reached. The decision lacks
-- sufficient permissions. One of the following events might be added to the
-- history to indicate an error. The event attribute's cause parameter
-- indicates the cause. If cause is set to OPERATION_NOT_PERMITTED, the
-- decision failed because it lacked sufficient permissions.
-- ScheduleActivityTaskFailed a ScheduleActivityTask decision failed. This
-- could happen if the activity type specified in the decision is not
-- registered, is in a deprecated state, or the decision is not properly
-- configured. RequestCancelActivityTaskFailed a RequestCancelActivityTask
-- decision failed. This could happen if there is no open activity task with
-- the specified activityId. StartTimerFailed a StartTimer decision failed.
-- This could happen if there is another open timer with the same timerId.
-- CancelTimerFailed a CancelTimer decision failed. This could happen if there
-- is no open timer with the specified timerId.
-- StartChildWorkflowExecutionFailed a StartChildWorkflowExecution decision
-- failed. This could happen if the workflow type specified is not registered,
-- is deprecated, or the decision is not properly configured.
-- SignalExternalWorkflowExecutionFailed a SignalExternalWorkflowExecution
-- decision failed. This could happen if the workflowID specified in the
-- decision was incorrect. RequestCancelExternalWorkflowExecutionFailed a
-- RequestCancelExternalWorkflowExecution decision failed. This could happen
-- if the workflowID specified in the decision was incorrect.
-- CancelWorkflowExecutionFailed a CancelWorkflowExecution decision failed.
-- This could happen if there is an unhandled decision task pending in the
-- workflow execution. CompleteWorkflowExecutionFailed a
-- CompleteWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution.
-- ContinueAsNewWorkflowExecutionFailed a ContinueAsNewWorkflowExecution
-- decision failed. This could happen if there is an unhandled decision task
-- pending in the workflow execution or the ContinueAsNewWorkflowExecution
-- decision was not configured correctly. FailWorkflowExecutionFailed a
-- FailWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution. The preceding
-- error events might occur due to an error in the decider logic, which might
-- put the workflow execution in an unstable state The cause field in the
-- event structure for the error event indicates the cause of the error. A
-- workflow execution may be closed by the decider by returning one of the
-- following decisions when completing a decision task:
-- CompleteWorkflowExecution, FailWorkflowExecution, CancelWorkflowExecution
-- and ContinueAsNewWorkflowExecution. An UnhandledDecision fault will be
-- returned if a workflow closing decision is specified and a signal or
-- activity event had been added to the history while the decision task was
-- being performed by the decider. Unlike the above situations which are logic
-- issues, this fault is always possible because of race conditions in a
-- distributed system. The right action here is to call
-- RespondDecisionTaskCompleted without any decisions. This would result in
-- another decision task with these new events included in the history. The
-- decider should handle the new events and may decide to close the workflow
-- execution. How to Code a Decision You code a decision by first setting the
-- decision type field to one of the above decision values, and then set the
-- corresponding attributes field shown below:
-- ScheduleActivityTaskDecisionAttributes
-- RequestCancelActivityTaskDecisionAttributes
-- CompleteWorkflowExecutionDecisionAttributes
-- FailWorkflowExecutionDecisionAttributes
-- CancelWorkflowExecutionDecisionAttributes
-- ContinueAsNewWorkflowExecutionDecisionAttributes
-- RecordMarkerDecisionAttributes StartTimerDecisionAttributes
-- CancelTimerDecisionAttributes
-- SignalExternalWorkflowExecutionDecisionAttributes
-- RequestCancelExternalWorkflowExecutionDecisionAttributes
-- StartChildWorkflowExecutionDecisionAttributes.
data Decision = Decision
    { dCancelTimerDecisionAttributes :: Maybe CancelTimerDecisionAttributes
      -- ^ Provides details of the CancelTimer decision. It is not set for other
      -- decision types.
    , dCancelWorkflowExecutionDecisionAttributes :: Maybe CancelWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CancelWorkflowExecution decision. It is not set for
      -- other decision types.
    , dCompleteWorkflowExecutionDecisionAttributes :: Maybe CompleteWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CompleteWorkflowExecution decision. It is not set
      -- for other decision types.
    , dContinueAsNewWorkflowExecutionDecisionAttributes :: Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the ContinueAsNewWorkflowExecution decision. It is not
      -- set for other decision types.
    , dDecisionType :: !DecisionType
      -- ^ Specifies the type of the decision.
    , dFailWorkflowExecutionDecisionAttributes :: Maybe FailWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the FailWorkflowExecution decision. It is not set for
      -- other decision types.
    , dRecordMarkerDecisionAttributes :: Maybe RecordMarkerDecisionAttributes
      -- ^ Provides details of the RecordMarker decision. It is not set for other
      -- decision types.
    , dRequestCancelActivityTaskDecisionAttributes :: Maybe RequestCancelActivityTaskDecisionAttributes
      -- ^ Provides details of the RequestCancelActivityTask decision. It is not set
      -- for other decision types.
    , dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the RequestCancelExternalWorkflowExecution decision. It
      -- is not set for other decision types.
    , dScheduleActivityTaskDecisionAttributes :: Maybe ScheduleActivityTaskDecisionAttributes
      -- ^ Provides details of the ScheduleActivityTask decision. It is not set for
      -- other decision types.
    , dSignalExternalWorkflowExecutionDecisionAttributes :: Maybe SignalExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the SignalExternalWorkflowExecution decision. It is not
      -- set for other decision types.
    , dStartChildWorkflowExecutionDecisionAttributes :: Maybe StartChildWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the StartChildWorkflowExecution decision. It is not set
      -- for other decision types.
    , dStartTimerDecisionAttributes :: Maybe StartTimerDecisionAttributes
      -- ^ Provides details of the StartTimer decision. It is not set for other
      -- decision types.
    } deriving (Eq, Show, Generic)

instance FromJSON Decision
instance ToJSON Decision

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { canwefeaCause :: !ContinueAsNewWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the system and
      -- can be useful for diagnostic purposes. If cause is set to
      -- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
      -- permissions. For details and example IAM policies, see Using IAM to Manage
      -- Access to Amazon SWF Workflows.
    , canwefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the ContinueAsNewWorkflowExecution decision that
      -- started this execution. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON ContinueAsNewWorkflowExecutionFailedEventAttributes
instance ToJSON ContinueAsNewWorkflowExecutionFailedEventAttributes

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { canwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow executions of
      -- the new execution if it is terminated by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired timeout.
      -- This policy overrides the default child policy specified when registering
      -- the workflow type using RegisterWorkflowType. The supported child policies
      -- are: TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up to the
      -- decider to take appropriate actions when it receives an execution history
      -- with this event. ABANDON: no action will be taken. The child executions
      -- will continue to run. A child policy for the new workflow execution must be
      -- specified either as a default registered for its workflow type or through
      -- this field. If neither this field is set nor a default child policy was
      -- specified at registration time then a fault will be returned.
    , canwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the total duration for this workflow execution. This
      -- overrides the defaultExecutionStartToCloseTimeout specified when
      -- registering the workflow type. The valid values are integers greater than
      -- or equal to 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration. An execution
      -- start-to-close timeout for this workflow execution must be specified either
      -- as a default for the workflow type or through this field. If neither this
      -- field is set nor a default execution start-to-close timeout was specified
      -- at registration time then a fault will be returned.
    , canwedaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , canwedaTagList :: [Text]
      -- ^ The list of tags to associate with the new workflow execution. A maximum of
      -- 5 tags can be specified. You can list workflow executions with a specific
      -- tag by calling ListOpenWorkflowExecutions or ListClosedWorkflowExecutions
      -- and specifying a TagFilter.
    , canwedaTaskList :: Maybe TaskList
      -- ^ Represents a task list.
    , canwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for the new workflow
      -- execution. This parameter overrides the defaultTaskStartToCloseTimout
      -- specified when registering the workflow type using RegisterWorkflowType.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for the new
      -- workflow execution must be specified either as a default for the workflow
      -- type or through this parameter. If neither this parameter is set nor a
      -- default task start-to-close timeout was specified at registration time then
      -- a fault will be returned.
    , canwedaWorkflowTypeVersion :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON ContinueAsNewWorkflowExecutionDecisionAttributes
instance ToJSON ContinueAsNewWorkflowExecutionDecisionAttributes

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes
    { cwefecCause :: !CompleteWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the system and
      -- can be useful for diagnostic purposes. If cause is set to
      -- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
      -- permissions. For details and example IAM policies, see Using IAM to Manage
      -- Access to Amazon SWF Workflows.
    , cwefecDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CompleteWorkflowExecution decision to complete
      -- this execution. This information can be useful for diagnosing problems by
      -- tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON CompleteWorkflowExecutionFailedEventAttributes
instance ToJSON CompleteWorkflowExecutionFailedEventAttributes

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { cwedbResult :: Maybe Text
      -- ^ The result of the workflow execution. The form of the result is
      -- implementation defined.
    } deriving (Eq, Show, Generic)

instance FromJSON CompleteWorkflowExecutionDecisionAttributes
instance ToJSON CompleteWorkflowExecutionDecisionAttributes

-- | If specified, only workflow executions that match this close status are
-- listed. For example, if TERMINATED is specified, then only TERMINATED
-- workflow executions are listed. closeStatusFilter, executionFilter,
-- typeFilter and tagFilter are mutually exclusive. You can specify at most
-- one of these in a request.
newtype CloseStatusFilter = CloseStatusFilter
    { csfStatus :: CloseStatus
      -- ^ The close status that must match the close status of an execution for it to
      -- meet the criteria of this filter. This field is required.
    } deriving (Eq, Show, Generic)

instance FromJSON CloseStatusFilter
instance ToJSON CloseStatusFilter

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes
    { cwetoeaInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cwetoeaStartedEventId :: !Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when this child
      -- workflow execution was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , cwetoeaTimeoutType :: !WorkflowExecutionTimeoutType
      -- ^ The type of the timeout that caused the child workflow execution to time
      -- out.
    , cwetoeaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that timed out.
    , cwetoeaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionTimedOutEventAttributes
instance ToJSON ChildWorkflowExecutionTimedOutEventAttributes

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes
    { cweteaInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cweteaStartedEventId :: !Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when this child
      -- workflow execution was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , cweteaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was terminated.
    , cweteaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionTerminatedEventAttributes
instance ToJSON ChildWorkflowExecutionTerminatedEventAttributes

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes
    { cweseaInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cweseaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was started.
    , cweseaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionStartedEventAttributes
instance ToJSON ChildWorkflowExecutionStartedEventAttributes

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes
    { cwefebDetails :: Maybe Text
      -- ^ The details of the failure (if provided).
    , cwefebInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cwefebReason :: Maybe Text
      -- ^ The reason for the failure (if provided).
    , cwefebStartedEventId :: !Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when this child
      -- workflow execution was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , cwefebWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that failed.
    , cwefebWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionFailedEventAttributes
instance ToJSON ChildWorkflowExecutionFailedEventAttributes

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes
    { cwecebInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cwecebResult :: Maybe Text
      -- ^ The result of the child workflow execution (if any).
    , cwecebStartedEventId :: !Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when this child
      -- workflow execution was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , cwecebWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was completed.
    , cwecebWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionCompletedEventAttributes
instance ToJSON ChildWorkflowExecutionCompletedEventAttributes

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes
    { cweceaDetails :: Maybe Text
      -- ^ Details of the cancellation (if provided).
    , cweceaInitiatedEventId :: !Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event corresponding to
      -- the StartChildWorkflowExecution Decision to start this child workflow
      -- execution. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , cweceaStartedEventId :: !Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when this child
      -- workflow execution was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , cweceaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was canceled.
    , cweceaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    } deriving (Eq, Show, Generic)

instance FromJSON ChildWorkflowExecutionCanceledEventAttributes
instance ToJSON ChildWorkflowExecutionCanceledEventAttributes

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes
    { cwefeaCause :: !CancelWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the system and
      -- can be useful for diagnostic purposes. If cause is set to
      -- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
      -- permissions. For details and example IAM policies, see Using IAM to Manage
      -- Access to Amazon SWF Workflows.
    , cwefeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CancelWorkflowExecution decision for this
      -- cancellation request. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON CancelWorkflowExecutionFailedEventAttributes
instance ToJSON CancelWorkflowExecutionFailedEventAttributes

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { cwedaDetails :: Maybe Text
      -- ^ Optional details of the cancellation.
    } deriving (Eq, Show, Generic)

instance FromJSON CancelWorkflowExecutionDecisionAttributes
instance ToJSON CancelWorkflowExecutionDecisionAttributes

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes
    { ctfeaCause :: !CancelTimerFailedCause
      -- ^ The cause of the failure to process the decision. This information is
      -- generated by the system and can be useful for diagnostic purposes. If cause
      -- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see Using IAM
      -- to Manage Access to Amazon SWF Workflows.
    , ctfeaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the CancelTimer decision to cancel this timer. This
      -- information can be useful for diagnosing problems by tracing back the cause
      -- of events.
    , ctfeaTimerId :: !Text
      -- ^ The timerId provided in the CancelTimer decision that failed.
    } deriving (Eq, Show, Generic)

instance FromJSON CancelTimerFailedEventAttributes
instance ToJSON CancelTimerFailedEventAttributes

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes
    { ctdaTimerId :: Text
      -- ^ The unique Id of the timer to cancel. This field is required.
    } deriving (Eq, Show, Generic)

instance FromJSON CancelTimerDecisionAttributes
instance ToJSON CancelTimerDecisionAttributes

-- | Detailed information about an activity type.
data ActivityTypeInfo = ActivityTypeInfo
    { atiActivityType :: ActivityType
      -- ^ The ActivityType type structure representing the activity type.
    , atiCreationDate :: !UTCTime
      -- ^ The date and time this activity type was created through
      -- RegisterActivityType.
    , atiDeprecationDate :: Maybe UTCTime
      -- ^ If DEPRECATED, the date and time DeprecateActivityType was called.
    , atiDescription :: Maybe Text
      -- ^ The description of the activity type provided in RegisterActivityType.
    , atiStatus :: !RegistrationStatus
      -- ^ The current status of the activity type.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTypeInfo
instance ToJSON ActivityTypeInfo

-- | The configuration settings registered with the activity type.
data ActivityTypeConfiguration = ActivityTypeConfiguration
    { atcDefaultTaskHeartbeatTimeout :: Maybe Text
      -- ^ The optional default maximum time, specified when registering the activity
      -- type, before which a worker processing a task must report progress by
      -- calling RecordActivityTaskHeartbeat. You can override this default when
      -- scheduling a task through the ScheduleActivityTask Decision. If the
      -- activity worker subsequently attempts to record a heartbeat or returns a
      -- result, the activity worker receives an UnknownResource fault. In this
      -- case, Amazon SWF no longer considers the activity task to be valid; the
      -- activity worker should clean up the activity task. The valid values are
      -- integers greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify unlimited
      -- duration.
    , atcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list specified for this activity type at
      -- registration. This default task list is used if a task list is not provided
      -- when a task is scheduled through the ScheduleActivityTask Decision. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision.
    , atcDefaultTaskScheduleToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering the
      -- activity type, for tasks of this activity type. You can override this
      -- default when scheduling a task through the ScheduleActivityTask Decision.
      -- The valid values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , atcDefaultTaskScheduleToStartTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering the
      -- activity type, that a task of an activity type can wait before being
      -- assigned to a worker. You can override this default when scheduling a task
      -- through the ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to specify the
      -- duration in seconds while NONE can be used to specify unlimited duration.
    , atcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration for tasks of an activity type
      -- specified when registering the activity type. You can override this default
      -- when scheduling a task through the ScheduleActivityTask Decision. The valid
      -- values are integers greater than or equal to 0. An integer value can be
      -- used to specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTypeConfiguration
instance ToJSON ActivityTypeConfiguration

-- | The ActivityType type structure representing the activity type.
data ActivityType = ActivityType
    { atName :: !Text
      -- ^ The name of this activity. The combination of activity type name and
      -- version must be unique within a domain.
    , atVersion :: !Text
      -- ^ The version of this activity. The combination of activity type name and
      -- version must be unique with in a domain.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityType
instance ToJSON ActivityType

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes
    { attoeaDetails :: Maybe Text
      -- ^ Contains the content of the details parameter for the last call made by the
      -- activity to RecordActivityTaskHeartbeat.
    , attoeaScheduledEventId :: !Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when this
      -- activity task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , attoeaStartedEventId :: !Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this activity task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    , attoeaTimeoutType :: !ActivityTaskTimeoutType
      -- ^ The type of the timeout that caused this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskTimedOutEventAttributes
instance ToJSON ActivityTaskTimedOutEventAttributes

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes
    { atsebIdentity :: Maybe Text
      -- ^ Identity of the worker that was assigned this task. This aids diagnostics
      -- when problems arise. The form of this identity is user defined.
    , atsebScheduledEventId :: !Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when this
      -- activity task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskStartedEventAttributes
instance ToJSON ActivityTaskStartedEventAttributes

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes
    { atseaActivityId :: !Text
      -- ^ The unique id of the activity task.
    , atseaActivityType :: ActivityType
      -- ^ The type of the activity task.
    , atseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the decider in
      -- subsequent workflow tasks. This data is not sent to the activity.
    , atseaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- that resulted in the scheduling of this activity task. This information can
      -- be useful for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , atseaHeartbeatTimeout :: Maybe Text
      -- ^ The maximum time before which the worker processing this task must report
      -- progress by calling RecordActivityTaskHeartbeat. If the timeout is
      -- exceeded, the activity task is automatically timed out. If the worker
      -- subsequently attempts to record a heartbeat or return a result, it will be
      -- ignored.
    , atseaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , atseaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time for this activity task.
    , atseaScheduleToStartTimeout :: Maybe Text
      -- ^ The maximum amount of time the activity task can wait to be assigned to a
      -- worker.
    , atseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time a worker may take to process the activity task.
    , atseaTaskList :: TaskList
      -- ^ The task list in which the activity task has been scheduled.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskScheduledEventAttributes
instance ToJSON ActivityTaskScheduledEventAttributes

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes
    { atfeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , atfeaReason :: Maybe Text
      -- ^ The reason provided for the failure (if any).
    , atfeaScheduledEventId :: !Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when this
      -- activity task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , atfeaStartedEventId :: !Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this activity task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskFailedEventAttributes
instance ToJSON ActivityTaskFailedEventAttributes

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes
    { atcebResult :: Maybe Text
      -- ^ The results of the activity task (if any).
    , atcebScheduledEventId :: !Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when this
      -- activity task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , atcebStartedEventId :: !Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this activity task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskCompletedEventAttributes
instance ToJSON ActivityTaskCompletedEventAttributes

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes
    { atceaDetails :: Maybe Text
      -- ^ Details of the cancellation (if any).
    , atceaLatestCancelRequestedEventId :: Maybe Integer
      -- ^ If set, contains the Id of the last ActivityTaskCancelRequested event
      -- recorded for this activity task. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading up to this
      -- event.
    , atceaScheduledEventId :: !Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when this
      -- activity task was scheduled. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this event.
    , atceaStartedEventId :: !Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this activity task
      -- was started. This information can be useful for diagnosing problems by
      -- tracing back the chain of events leading up to this event.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskCanceledEventAttributes
instance ToJSON ActivityTaskCanceledEventAttributes

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes
    { atcreaActivityId :: !Text
      -- ^ The unique ID of the task.
    , atcreaDecisionTaskCompletedEventId :: !Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the decision
      -- task that resulted in the RequestCancelActivityTask decision for this
      -- cancellation request. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Eq, Show, Generic)

instance FromJSON ActivityTaskCancelRequestedEventAttributes
instance ToJSON ActivityTaskCancelRequestedEventAttributes

-- | The type of the timeout that caused the child workflow execution to time
-- out.

data WorkflowExecutionTimeoutType
    = WorkflowExecutionTimeoutTypeSTART_TO_CLOSE
      deriving (Eq, Ord, Generic)

instance Hashable WorkflowExecutionTimeoutType

instance FromText WorkflowExecutionTimeoutType where
    fromText "START_TO_CLOSE" = Right WorkflowExecutionTimeoutTypeSTART_TO_CLOSE
    fromText e = fromTextFail $ "Unrecognised WorkflowExecutionTimeoutType: " <> e

instance Read WorkflowExecutionTimeoutType where
    readsPrec _ = fromTextRead

instance ToText WorkflowExecutionTimeoutType where
    toText WorkflowExecutionTimeoutTypeSTART_TO_CLOSE = "START_TO_CLOSE"

instance Show WorkflowExecutionTimeoutType where
    show = toTextShow

instance FromJSON WorkflowExecutionTimeoutType where
    parseJSON = fromTextJSON "WorkflowExecutionTimeoutType"

instance FromJSON v => FromJSON (HashMap WorkflowExecutionTimeoutType v) where
    parseJSON = fromTextHashJSON

instance ToJSON WorkflowExecutionTimeoutType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap WorkflowExecutionTimeoutType v) where
    toJSON = toTextHashJSON

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.

data WorkflowExecutionTerminatedCause
    = WorkflowExecutionTerminatedCauseCHILD_POLICY_APPLIED
    | WorkflowExecutionTerminatedCauseEVENT_LIMIT_EXCEEDED
    | WorkflowExecutionTerminatedCauseOPERATOR_INITIATED
      deriving (Eq, Ord, Generic)

instance Hashable WorkflowExecutionTerminatedCause

instance FromText WorkflowExecutionTerminatedCause where
    fromText "CHILD_POLICY_APPLIED" = Right WorkflowExecutionTerminatedCauseCHILD_POLICY_APPLIED
    fromText "EVENT_LIMIT_EXCEEDED" = Right WorkflowExecutionTerminatedCauseEVENT_LIMIT_EXCEEDED
    fromText "OPERATOR_INITIATED" = Right WorkflowExecutionTerminatedCauseOPERATOR_INITIATED
    fromText e = fromTextFail $ "Unrecognised WorkflowExecutionTerminatedCause: " <> e

instance Read WorkflowExecutionTerminatedCause where
    readsPrec _ = fromTextRead

instance ToText WorkflowExecutionTerminatedCause where
    toText WorkflowExecutionTerminatedCauseCHILD_POLICY_APPLIED = "CHILD_POLICY_APPLIED"
    toText WorkflowExecutionTerminatedCauseEVENT_LIMIT_EXCEEDED = "EVENT_LIMIT_EXCEEDED"
    toText WorkflowExecutionTerminatedCauseOPERATOR_INITIATED = "OPERATOR_INITIATED"

instance Show WorkflowExecutionTerminatedCause where
    show = toTextShow

instance FromJSON WorkflowExecutionTerminatedCause where
    parseJSON = fromTextJSON "WorkflowExecutionTerminatedCause"

instance FromJSON v => FromJSON (HashMap WorkflowExecutionTerminatedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON WorkflowExecutionTerminatedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap WorkflowExecutionTerminatedCause v) where
    toJSON = toTextHashJSON

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.

data WorkflowExecutionCancelRequestedCause
    = WorkflowExecutionCancelRequestedCauseCHILD_POLICY_APPLIED
      deriving (Eq, Ord, Generic)

instance Hashable WorkflowExecutionCancelRequestedCause

instance FromText WorkflowExecutionCancelRequestedCause where
    fromText "CHILD_POLICY_APPLIED" = Right WorkflowExecutionCancelRequestedCauseCHILD_POLICY_APPLIED
    fromText e = fromTextFail $ "Unrecognised WorkflowExecutionCancelRequestedCause: " <> e

instance Read WorkflowExecutionCancelRequestedCause where
    readsPrec _ = fromTextRead

instance ToText WorkflowExecutionCancelRequestedCause where
    toText WorkflowExecutionCancelRequestedCauseCHILD_POLICY_APPLIED = "CHILD_POLICY_APPLIED"

instance Show WorkflowExecutionCancelRequestedCause where
    show = toTextShow

instance FromJSON WorkflowExecutionCancelRequestedCause where
    parseJSON = fromTextJSON "WorkflowExecutionCancelRequestedCause"

instance FromJSON v => FromJSON (HashMap WorkflowExecutionCancelRequestedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON WorkflowExecutionCancelRequestedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap WorkflowExecutionCancelRequestedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data StartTimerFailedCause
    = StartTimerFailedCauseOPEN_TIMERS_LIMIT_EXCEEDED
    | StartTimerFailedCauseOPERATION_NOT_PERMITTED
    | StartTimerFailedCauseTIMER_CREATION_RATE_EXCEEDED
    | StartTimerFailedCauseTIMER_ID_ALREADY_IN_USE
      deriving (Eq, Ord, Generic)

instance Hashable StartTimerFailedCause

instance FromText StartTimerFailedCause where
    fromText "OPEN_TIMERS_LIMIT_EXCEEDED" = Right StartTimerFailedCauseOPEN_TIMERS_LIMIT_EXCEEDED
    fromText "OPERATION_NOT_PERMITTED" = Right StartTimerFailedCauseOPERATION_NOT_PERMITTED
    fromText "TIMER_CREATION_RATE_EXCEEDED" = Right StartTimerFailedCauseTIMER_CREATION_RATE_EXCEEDED
    fromText "TIMER_ID_ALREADY_IN_USE" = Right StartTimerFailedCauseTIMER_ID_ALREADY_IN_USE
    fromText e = fromTextFail $ "Unrecognised StartTimerFailedCause: " <> e

instance Read StartTimerFailedCause where
    readsPrec _ = fromTextRead

instance ToText StartTimerFailedCause where
    toText StartTimerFailedCauseOPEN_TIMERS_LIMIT_EXCEEDED = "OPEN_TIMERS_LIMIT_EXCEEDED"
    toText StartTimerFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText StartTimerFailedCauseTIMER_CREATION_RATE_EXCEEDED = "TIMER_CREATION_RATE_EXCEEDED"
    toText StartTimerFailedCauseTIMER_ID_ALREADY_IN_USE = "TIMER_ID_ALREADY_IN_USE"

instance Show StartTimerFailedCause where
    show = toTextShow

instance FromJSON StartTimerFailedCause where
    parseJSON = fromTextJSON "StartTimerFailedCause"

instance FromJSON v => FromJSON (HashMap StartTimerFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON StartTimerFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StartTimerFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data StartChildWorkflowExecutionFailedCause
    = StartChildWorkflowExecutionFailedCauseCHILD_CREATION_RATE_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseOPEN_CHILDREN_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOPEN_WORKFLOWS_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | StartChildWorkflowExecutionFailedCauseWORKFLOW_ALREADY_RUNNING
    | StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED
    | StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Ord, Generic)

instance Hashable StartChildWorkflowExecutionFailedCause

instance FromText StartChildWorkflowExecutionFailedCause where
    fromText "CHILD_CREATION_RATE_EXCEEDED" = Right StartChildWorkflowExecutionFailedCauseCHILD_CREATION_RATE_EXCEEDED
    fromText "DEFAULT_CHILD_POLICY_UNDEFINED" = Right StartChildWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED
    fromText "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" = Right StartChildWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "DEFAULT_TASK_LIST_UNDEFINED" = Right StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    fromText "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" = Right StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "OPEN_CHILDREN_LIMIT_EXCEEDED" = Right StartChildWorkflowExecutionFailedCauseOPEN_CHILDREN_LIMIT_EXCEEDED
    fromText "OPEN_WORKFLOWS_LIMIT_EXCEEDED" = Right StartChildWorkflowExecutionFailedCauseOPEN_WORKFLOWS_LIMIT_EXCEEDED
    fromText "OPERATION_NOT_PERMITTED" = Right StartChildWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "WORKFLOW_ALREADY_RUNNING" = Right StartChildWorkflowExecutionFailedCauseWORKFLOW_ALREADY_RUNNING
    fromText "WORKFLOW_TYPE_DEPRECATED" = Right StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED
    fromText "WORKFLOW_TYPE_DOES_NOT_EXIST" = Right StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST
    fromText e = fromTextFail $ "Unrecognised StartChildWorkflowExecutionFailedCause: " <> e

instance Read StartChildWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText StartChildWorkflowExecutionFailedCause where
    toText StartChildWorkflowExecutionFailedCauseCHILD_CREATION_RATE_EXCEEDED = "CHILD_CREATION_RATE_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED = "DEFAULT_TASK_LIST_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseOPEN_CHILDREN_LIMIT_EXCEEDED = "OPEN_CHILDREN_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOPEN_WORKFLOWS_LIMIT_EXCEEDED = "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText StartChildWorkflowExecutionFailedCauseWORKFLOW_ALREADY_RUNNING = "WORKFLOW_ALREADY_RUNNING"
    toText StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED = "WORKFLOW_TYPE_DEPRECATED"
    toText StartChildWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Show StartChildWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON StartChildWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "StartChildWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap StartChildWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON StartChildWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StartChildWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data SignalExternalWorkflowExecutionFailedCause
    = SignalExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | SignalExternalWorkflowExecutionFailedCauseSIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | SignalExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Ord, Generic)

instance Hashable SignalExternalWorkflowExecutionFailedCause

instance FromText SignalExternalWorkflowExecutionFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right SignalExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" = Right SignalExternalWorkflowExecutionFailedCauseSIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    fromText "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" = Right SignalExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
    fromText e = fromTextFail $ "Unrecognised SignalExternalWorkflowExecutionFailedCause: " <> e

instance Read SignalExternalWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText SignalExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText SignalExternalWorkflowExecutionFailedCauseSIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED = "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText SignalExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Show SignalExternalWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "SignalExternalWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap SignalExternalWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON SignalExternalWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap SignalExternalWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data ScheduleActivityTaskFailedCause
    = ScheduleActivityTaskFailedCauseACTIVITY_CREATION_RATE_EXCEEDED
    | ScheduleActivityTaskFailedCauseACTIVITY_ID_ALREADY_IN_USE
    | ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DEPRECATED
    | ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DOES_NOT_EXIST
    | ScheduleActivityTaskFailedCauseDEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    | ScheduleActivityTaskFailedCauseOPEN_ACTIVITIES_LIMIT_EXCEEDED
    | ScheduleActivityTaskFailedCauseOPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Generic)

instance Hashable ScheduleActivityTaskFailedCause

instance FromText ScheduleActivityTaskFailedCause where
    fromText "ACTIVITY_CREATION_RATE_EXCEEDED" = Right ScheduleActivityTaskFailedCauseACTIVITY_CREATION_RATE_EXCEEDED
    fromText "ACTIVITY_ID_ALREADY_IN_USE" = Right ScheduleActivityTaskFailedCauseACTIVITY_ID_ALREADY_IN_USE
    fromText "ACTIVITY_TYPE_DEPRECATED" = Right ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DEPRECATED
    fromText "ACTIVITY_TYPE_DOES_NOT_EXIST" = Right ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DOES_NOT_EXIST
    fromText "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED" = Right ScheduleActivityTaskFailedCauseDEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED
    fromText "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" = Right ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED" = Right ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED
    fromText "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED" = Right ScheduleActivityTaskFailedCauseDEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "DEFAULT_TASK_LIST_UNDEFINED" = Right ScheduleActivityTaskFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    fromText "OPEN_ACTIVITIES_LIMIT_EXCEEDED" = Right ScheduleActivityTaskFailedCauseOPEN_ACTIVITIES_LIMIT_EXCEEDED
    fromText "OPERATION_NOT_PERMITTED" = Right ScheduleActivityTaskFailedCauseOPERATION_NOT_PERMITTED
    fromText e = fromTextFail $ "Unrecognised ScheduleActivityTaskFailedCause: " <> e

instance Read ScheduleActivityTaskFailedCause where
    readsPrec _ = fromTextRead

instance ToText ScheduleActivityTaskFailedCause where
    toText ScheduleActivityTaskFailedCauseACTIVITY_CREATION_RATE_EXCEEDED = "ACTIVITY_CREATION_RATE_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseACTIVITY_ID_ALREADY_IN_USE = "ACTIVITY_ID_ALREADY_IN_USE"
    toText ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DEPRECATED = "ACTIVITY_TYPE_DEPRECATED"
    toText ScheduleActivityTaskFailedCauseACTIVITY_TYPE_DOES_NOT_EXIST = "ACTIVITY_TYPE_DOES_NOT_EXIST"
    toText ScheduleActivityTaskFailedCauseDEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED = "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED = "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDEFAULT_TASK_LIST_UNDEFINED = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseOPEN_ACTIVITIES_LIMIT_EXCEEDED = "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"

instance Show ScheduleActivityTaskFailedCause where
    show = toTextShow

instance FromJSON ScheduleActivityTaskFailedCause where
    parseJSON = fromTextJSON "ScheduleActivityTaskFailedCause"

instance FromJSON v => FromJSON (HashMap ScheduleActivityTaskFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON ScheduleActivityTaskFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ScheduleActivityTaskFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data RequestCancelExternalWorkflowExecutionFailedCause
    = RequestCancelExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | RequestCancelExternalWorkflowExecutionFailedCauseREQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | RequestCancelExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Ord, Generic)

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right RequestCancelExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" = Right RequestCancelExternalWorkflowExecutionFailedCauseREQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    fromText "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" = Right RequestCancelExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
    fromText e = fromTextFail $ "Unrecognised RequestCancelExternalWorkflowExecutionFailedCause: " <> e

instance Read RequestCancelExternalWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText RequestCancelExternalWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseREQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED = "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseUNKNOWN_EXTERNAL_WORKFLOW_EXECUTION = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Show RequestCancelExternalWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "RequestCancelExternalWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap RequestCancelExternalWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON RequestCancelExternalWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap RequestCancelExternalWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data RequestCancelActivityTaskFailedCause
    = RequestCancelActivityTaskFailedCauseACTIVITY_ID_UNKNOWN
    | RequestCancelActivityTaskFailedCauseOPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Generic)

instance Hashable RequestCancelActivityTaskFailedCause

instance FromText RequestCancelActivityTaskFailedCause where
    fromText "ACTIVITY_ID_UNKNOWN" = Right RequestCancelActivityTaskFailedCauseACTIVITY_ID_UNKNOWN
    fromText "OPERATION_NOT_PERMITTED" = Right RequestCancelActivityTaskFailedCauseOPERATION_NOT_PERMITTED
    fromText e = fromTextFail $ "Unrecognised RequestCancelActivityTaskFailedCause: " <> e

instance Read RequestCancelActivityTaskFailedCause where
    readsPrec _ = fromTextRead

instance ToText RequestCancelActivityTaskFailedCause where
    toText RequestCancelActivityTaskFailedCauseACTIVITY_ID_UNKNOWN = "ACTIVITY_ID_UNKNOWN"
    toText RequestCancelActivityTaskFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"

instance Show RequestCancelActivityTaskFailedCause where
    show = toTextShow

instance FromJSON RequestCancelActivityTaskFailedCause where
    parseJSON = fromTextJSON "RequestCancelActivityTaskFailedCause"

instance FromJSON v => FromJSON (HashMap RequestCancelActivityTaskFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON RequestCancelActivityTaskFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap RequestCancelActivityTaskFailedCause v) where
    toJSON = toTextHashJSON

-- | Specifies the registration status of the activity types to list.

data RegistrationStatus
    = RegistrationStatusDEPRECATED
    | RegistrationStatusREGISTERED
      deriving (Eq, Ord, Generic)

instance Hashable RegistrationStatus

instance FromText RegistrationStatus where
    fromText "DEPRECATED" = Right RegistrationStatusDEPRECATED
    fromText "REGISTERED" = Right RegistrationStatusREGISTERED
    fromText e = fromTextFail $ "Unrecognised RegistrationStatus: " <> e

instance Read RegistrationStatus where
    readsPrec _ = fromTextRead

instance ToText RegistrationStatus where
    toText RegistrationStatusDEPRECATED = "DEPRECATED"
    toText RegistrationStatusREGISTERED = "REGISTERED"

instance Show RegistrationStatus where
    show = toTextShow

instance FromJSON RegistrationStatus where
    parseJSON = fromTextJSON "RegistrationStatus"

instance FromJSON v => FromJSON (HashMap RegistrationStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON RegistrationStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap RegistrationStatus v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data RecordMarkerFailedCause
    = RecordMarkerFailedCauseOPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Generic)

instance Hashable RecordMarkerFailedCause

instance FromText RecordMarkerFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right RecordMarkerFailedCauseOPERATION_NOT_PERMITTED
    fromText e = fromTextFail $ "Unrecognised RecordMarkerFailedCause: " <> e

instance Read RecordMarkerFailedCause where
    readsPrec _ = fromTextRead

instance ToText RecordMarkerFailedCause where
    toText RecordMarkerFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"

instance Show RecordMarkerFailedCause where
    show = toTextShow

instance FromJSON RecordMarkerFailedCause where
    parseJSON = fromTextJSON "RecordMarkerFailedCause"

instance FromJSON v => FromJSON (HashMap RecordMarkerFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON RecordMarkerFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap RecordMarkerFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.

data FailWorkflowExecutionFailedCause
    = FailWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | FailWorkflowExecutionFailedCauseUNHANDLED_DECISION
      deriving (Eq, Ord, Generic)

instance Hashable FailWorkflowExecutionFailedCause

instance FromText FailWorkflowExecutionFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right FailWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "UNHANDLED_DECISION" = Right FailWorkflowExecutionFailedCauseUNHANDLED_DECISION
    fromText e = fromTextFail $ "Unrecognised FailWorkflowExecutionFailedCause: " <> e

instance Read FailWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText FailWorkflowExecutionFailedCause where
    toText FailWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText FailWorkflowExecutionFailedCauseUNHANDLED_DECISION = "UNHANDLED_DECISION"

instance Show FailWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON FailWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "FailWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap FailWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON FailWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap FailWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The current status of the execution.

data ExecutionStatus
    = ExecutionStatusCLOSED
    | ExecutionStatusOPEN
      deriving (Eq, Ord, Generic)

instance Hashable ExecutionStatus

instance FromText ExecutionStatus where
    fromText "CLOSED" = Right ExecutionStatusCLOSED
    fromText "OPEN" = Right ExecutionStatusOPEN
    fromText e = fromTextFail $ "Unrecognised ExecutionStatus: " <> e

instance Read ExecutionStatus where
    readsPrec _ = fromTextRead

instance ToText ExecutionStatus where
    toText ExecutionStatusCLOSED = "CLOSED"
    toText ExecutionStatusOPEN = "OPEN"

instance Show ExecutionStatus where
    show = toTextShow

instance FromJSON ExecutionStatus where
    parseJSON = fromTextJSON "ExecutionStatus"

instance FromJSON v => FromJSON (HashMap ExecutionStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON ExecutionStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ExecutionStatus v) where
    toJSON = toTextHashJSON

-- | The type of the history event.

data EventType
    = EventTypeActivityTaskCancelRequested
    | EventTypeActivityTaskCanceled
    | EventTypeActivityTaskCompleted
    | EventTypeActivityTaskFailed
    | EventTypeActivityTaskScheduled
    | EventTypeActivityTaskStarted
    | EventTypeActivityTaskTimedOut
    | EventTypeCancelTimerFailed
    | EventTypeCancelWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionCanceled
    | EventTypeChildWorkflowExecutionCompleted
    | EventTypeChildWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionStarted
    | EventTypeChildWorkflowExecutionTerminated
    | EventTypeChildWorkflowExecutionTimedOut
    | EventTypeCompleteWorkflowExecutionFailed
    | EventTypeContinueAsNewWorkflowExecutionFailed
    | EventTypeDecisionTaskCompleted
    | EventTypeDecisionTaskScheduled
    | EventTypeDecisionTaskStarted
    | EventTypeDecisionTaskTimedOut
    | EventTypeExternalWorkflowExecutionCancelRequested
    | EventTypeExternalWorkflowExecutionSignaled
    | EventTypeFailWorkflowExecutionFailed
    | EventTypeMarkerRecorded
    | EventTypeRecordMarkerFailed
    | EventTypeRequestCancelActivityTaskFailed
    | EventTypeRequestCancelExternalWorkflowExecutionFailed
    | EventTypeRequestCancelExternalWorkflowExecutionInitiated
    | EventTypeScheduleActivityTaskFailed
    | EventTypeSignalExternalWorkflowExecutionFailed
    | EventTypeSignalExternalWorkflowExecutionInitiated
    | EventTypeStartChildWorkflowExecutionFailed
    | EventTypeStartChildWorkflowExecutionInitiated
    | EventTypeStartTimerFailed
    | EventTypeTimerCanceled
    | EventTypeTimerFired
    | EventTypeTimerStarted
    | EventTypeWorkflowExecutionCancelRequested
    | EventTypeWorkflowExecutionCanceled
    | EventTypeWorkflowExecutionCompleted
    | EventTypeWorkflowExecutionContinuedAsNew
    | EventTypeWorkflowExecutionFailed
    | EventTypeWorkflowExecutionSignaled
    | EventTypeWorkflowExecutionStarted
    | EventTypeWorkflowExecutionTerminated
    | EventTypeWorkflowExecutionTimedOut
      deriving (Eq, Ord, Generic)

instance Hashable EventType

instance FromText EventType where
    fromText "ActivityTaskCancelRequested" = Right EventTypeActivityTaskCancelRequested
    fromText "ActivityTaskCanceled" = Right EventTypeActivityTaskCanceled
    fromText "ActivityTaskCompleted" = Right EventTypeActivityTaskCompleted
    fromText "ActivityTaskFailed" = Right EventTypeActivityTaskFailed
    fromText "ActivityTaskScheduled" = Right EventTypeActivityTaskScheduled
    fromText "ActivityTaskStarted" = Right EventTypeActivityTaskStarted
    fromText "ActivityTaskTimedOut" = Right EventTypeActivityTaskTimedOut
    fromText "CancelTimerFailed" = Right EventTypeCancelTimerFailed
    fromText "CancelWorkflowExecutionFailed" = Right EventTypeCancelWorkflowExecutionFailed
    fromText "ChildWorkflowExecutionCanceled" = Right EventTypeChildWorkflowExecutionCanceled
    fromText "ChildWorkflowExecutionCompleted" = Right EventTypeChildWorkflowExecutionCompleted
    fromText "ChildWorkflowExecutionFailed" = Right EventTypeChildWorkflowExecutionFailed
    fromText "ChildWorkflowExecutionStarted" = Right EventTypeChildWorkflowExecutionStarted
    fromText "ChildWorkflowExecutionTerminated" = Right EventTypeChildWorkflowExecutionTerminated
    fromText "ChildWorkflowExecutionTimedOut" = Right EventTypeChildWorkflowExecutionTimedOut
    fromText "CompleteWorkflowExecutionFailed" = Right EventTypeCompleteWorkflowExecutionFailed
    fromText "ContinueAsNewWorkflowExecutionFailed" = Right EventTypeContinueAsNewWorkflowExecutionFailed
    fromText "DecisionTaskCompleted" = Right EventTypeDecisionTaskCompleted
    fromText "DecisionTaskScheduled" = Right EventTypeDecisionTaskScheduled
    fromText "DecisionTaskStarted" = Right EventTypeDecisionTaskStarted
    fromText "DecisionTaskTimedOut" = Right EventTypeDecisionTaskTimedOut
    fromText "ExternalWorkflowExecutionCancelRequested" = Right EventTypeExternalWorkflowExecutionCancelRequested
    fromText "ExternalWorkflowExecutionSignaled" = Right EventTypeExternalWorkflowExecutionSignaled
    fromText "FailWorkflowExecutionFailed" = Right EventTypeFailWorkflowExecutionFailed
    fromText "MarkerRecorded" = Right EventTypeMarkerRecorded
    fromText "RecordMarkerFailed" = Right EventTypeRecordMarkerFailed
    fromText "RequestCancelActivityTaskFailed" = Right EventTypeRequestCancelActivityTaskFailed
    fromText "RequestCancelExternalWorkflowExecutionFailed" = Right EventTypeRequestCancelExternalWorkflowExecutionFailed
    fromText "RequestCancelExternalWorkflowExecutionInitiated" = Right EventTypeRequestCancelExternalWorkflowExecutionInitiated
    fromText "ScheduleActivityTaskFailed" = Right EventTypeScheduleActivityTaskFailed
    fromText "SignalExternalWorkflowExecutionFailed" = Right EventTypeSignalExternalWorkflowExecutionFailed
    fromText "SignalExternalWorkflowExecutionInitiated" = Right EventTypeSignalExternalWorkflowExecutionInitiated
    fromText "StartChildWorkflowExecutionFailed" = Right EventTypeStartChildWorkflowExecutionFailed
    fromText "StartChildWorkflowExecutionInitiated" = Right EventTypeStartChildWorkflowExecutionInitiated
    fromText "StartTimerFailed" = Right EventTypeStartTimerFailed
    fromText "TimerCanceled" = Right EventTypeTimerCanceled
    fromText "TimerFired" = Right EventTypeTimerFired
    fromText "TimerStarted" = Right EventTypeTimerStarted
    fromText "WorkflowExecutionCancelRequested" = Right EventTypeWorkflowExecutionCancelRequested
    fromText "WorkflowExecutionCanceled" = Right EventTypeWorkflowExecutionCanceled
    fromText "WorkflowExecutionCompleted" = Right EventTypeWorkflowExecutionCompleted
    fromText "WorkflowExecutionContinuedAsNew" = Right EventTypeWorkflowExecutionContinuedAsNew
    fromText "WorkflowExecutionFailed" = Right EventTypeWorkflowExecutionFailed
    fromText "WorkflowExecutionSignaled" = Right EventTypeWorkflowExecutionSignaled
    fromText "WorkflowExecutionStarted" = Right EventTypeWorkflowExecutionStarted
    fromText "WorkflowExecutionTerminated" = Right EventTypeWorkflowExecutionTerminated
    fromText "WorkflowExecutionTimedOut" = Right EventTypeWorkflowExecutionTimedOut
    fromText e = fromTextFail $ "Unrecognised EventType: " <> e

instance Read EventType where
    readsPrec _ = fromTextRead

instance ToText EventType where
    toText EventTypeActivityTaskCancelRequested = "ActivityTaskCancelRequested"
    toText EventTypeActivityTaskCanceled = "ActivityTaskCanceled"
    toText EventTypeActivityTaskCompleted = "ActivityTaskCompleted"
    toText EventTypeActivityTaskFailed = "ActivityTaskFailed"
    toText EventTypeActivityTaskScheduled = "ActivityTaskScheduled"
    toText EventTypeActivityTaskStarted = "ActivityTaskStarted"
    toText EventTypeActivityTaskTimedOut = "ActivityTaskTimedOut"
    toText EventTypeCancelTimerFailed = "CancelTimerFailed"
    toText EventTypeCancelWorkflowExecutionFailed = "CancelWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionCanceled = "ChildWorkflowExecutionCanceled"
    toText EventTypeChildWorkflowExecutionCompleted = "ChildWorkflowExecutionCompleted"
    toText EventTypeChildWorkflowExecutionFailed = "ChildWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionStarted = "ChildWorkflowExecutionStarted"
    toText EventTypeChildWorkflowExecutionTerminated = "ChildWorkflowExecutionTerminated"
    toText EventTypeChildWorkflowExecutionTimedOut = "ChildWorkflowExecutionTimedOut"
    toText EventTypeCompleteWorkflowExecutionFailed = "CompleteWorkflowExecutionFailed"
    toText EventTypeContinueAsNewWorkflowExecutionFailed = "ContinueAsNewWorkflowExecutionFailed"
    toText EventTypeDecisionTaskCompleted = "DecisionTaskCompleted"
    toText EventTypeDecisionTaskScheduled = "DecisionTaskScheduled"
    toText EventTypeDecisionTaskStarted = "DecisionTaskStarted"
    toText EventTypeDecisionTaskTimedOut = "DecisionTaskTimedOut"
    toText EventTypeExternalWorkflowExecutionCancelRequested = "ExternalWorkflowExecutionCancelRequested"
    toText EventTypeExternalWorkflowExecutionSignaled = "ExternalWorkflowExecutionSignaled"
    toText EventTypeFailWorkflowExecutionFailed = "FailWorkflowExecutionFailed"
    toText EventTypeMarkerRecorded = "MarkerRecorded"
    toText EventTypeRecordMarkerFailed = "RecordMarkerFailed"
    toText EventTypeRequestCancelActivityTaskFailed = "RequestCancelActivityTaskFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionFailed = "RequestCancelExternalWorkflowExecutionFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionInitiated = "RequestCancelExternalWorkflowExecutionInitiated"
    toText EventTypeScheduleActivityTaskFailed = "ScheduleActivityTaskFailed"
    toText EventTypeSignalExternalWorkflowExecutionFailed = "SignalExternalWorkflowExecutionFailed"
    toText EventTypeSignalExternalWorkflowExecutionInitiated = "SignalExternalWorkflowExecutionInitiated"
    toText EventTypeStartChildWorkflowExecutionFailed = "StartChildWorkflowExecutionFailed"
    toText EventTypeStartChildWorkflowExecutionInitiated = "StartChildWorkflowExecutionInitiated"
    toText EventTypeStartTimerFailed = "StartTimerFailed"
    toText EventTypeTimerCanceled = "TimerCanceled"
    toText EventTypeTimerFired = "TimerFired"
    toText EventTypeTimerStarted = "TimerStarted"
    toText EventTypeWorkflowExecutionCancelRequested = "WorkflowExecutionCancelRequested"
    toText EventTypeWorkflowExecutionCanceled = "WorkflowExecutionCanceled"
    toText EventTypeWorkflowExecutionCompleted = "WorkflowExecutionCompleted"
    toText EventTypeWorkflowExecutionContinuedAsNew = "WorkflowExecutionContinuedAsNew"
    toText EventTypeWorkflowExecutionFailed = "WorkflowExecutionFailed"
    toText EventTypeWorkflowExecutionSignaled = "WorkflowExecutionSignaled"
    toText EventTypeWorkflowExecutionStarted = "WorkflowExecutionStarted"
    toText EventTypeWorkflowExecutionTerminated = "WorkflowExecutionTerminated"
    toText EventTypeWorkflowExecutionTimedOut = "WorkflowExecutionTimedOut"

instance Show EventType where
    show = toTextShow

instance FromJSON EventType where
    parseJSON = fromTextJSON "EventType"

instance FromJSON v => FromJSON (HashMap EventType v) where
    parseJSON = fromTextHashJSON

instance ToJSON EventType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap EventType v) where
    toJSON = toTextHashJSON

-- | Specifies the type of the decision.

data DecisionType
    = DecisionTypeCancelTimer
    | DecisionTypeCancelWorkflowExecution
    | DecisionTypeCompleteWorkflowExecution
    | DecisionTypeContinueAsNewWorkflowExecution
    | DecisionTypeFailWorkflowExecution
    | DecisionTypeRecordMarker
    | DecisionTypeRequestCancelActivityTask
    | DecisionTypeRequestCancelExternalWorkflowExecution
    | DecisionTypeScheduleActivityTask
    | DecisionTypeSignalExternalWorkflowExecution
    | DecisionTypeStartChildWorkflowExecution
    | DecisionTypeStartTimer
      deriving (Eq, Ord, Generic)

instance Hashable DecisionType

instance FromText DecisionType where
    fromText "CancelTimer" = Right DecisionTypeCancelTimer
    fromText "CancelWorkflowExecution" = Right DecisionTypeCancelWorkflowExecution
    fromText "CompleteWorkflowExecution" = Right DecisionTypeCompleteWorkflowExecution
    fromText "ContinueAsNewWorkflowExecution" = Right DecisionTypeContinueAsNewWorkflowExecution
    fromText "FailWorkflowExecution" = Right DecisionTypeFailWorkflowExecution
    fromText "RecordMarker" = Right DecisionTypeRecordMarker
    fromText "RequestCancelActivityTask" = Right DecisionTypeRequestCancelActivityTask
    fromText "RequestCancelExternalWorkflowExecution" = Right DecisionTypeRequestCancelExternalWorkflowExecution
    fromText "ScheduleActivityTask" = Right DecisionTypeScheduleActivityTask
    fromText "SignalExternalWorkflowExecution" = Right DecisionTypeSignalExternalWorkflowExecution
    fromText "StartChildWorkflowExecution" = Right DecisionTypeStartChildWorkflowExecution
    fromText "StartTimer" = Right DecisionTypeStartTimer
    fromText e = fromTextFail $ "Unrecognised DecisionType: " <> e

instance Read DecisionType where
    readsPrec _ = fromTextRead

instance ToText DecisionType where
    toText DecisionTypeCancelTimer = "CancelTimer"
    toText DecisionTypeCancelWorkflowExecution = "CancelWorkflowExecution"
    toText DecisionTypeCompleteWorkflowExecution = "CompleteWorkflowExecution"
    toText DecisionTypeContinueAsNewWorkflowExecution = "ContinueAsNewWorkflowExecution"
    toText DecisionTypeFailWorkflowExecution = "FailWorkflowExecution"
    toText DecisionTypeRecordMarker = "RecordMarker"
    toText DecisionTypeRequestCancelActivityTask = "RequestCancelActivityTask"
    toText DecisionTypeRequestCancelExternalWorkflowExecution = "RequestCancelExternalWorkflowExecution"
    toText DecisionTypeScheduleActivityTask = "ScheduleActivityTask"
    toText DecisionTypeSignalExternalWorkflowExecution = "SignalExternalWorkflowExecution"
    toText DecisionTypeStartChildWorkflowExecution = "StartChildWorkflowExecution"
    toText DecisionTypeStartTimer = "StartTimer"

instance Show DecisionType where
    show = toTextShow

instance FromJSON DecisionType where
    parseJSON = fromTextJSON "DecisionType"

instance FromJSON v => FromJSON (HashMap DecisionType v) where
    parseJSON = fromTextHashJSON

instance ToJSON DecisionType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap DecisionType v) where
    toJSON = toTextHashJSON

-- | The type of timeout that expired before the decision task could be
-- completed.

data DecisionTaskTimeoutType
    = DecisionTaskTimeoutTypeSTART_TO_CLOSE
      deriving (Eq, Ord, Generic)

instance Hashable DecisionTaskTimeoutType

instance FromText DecisionTaskTimeoutType where
    fromText "START_TO_CLOSE" = Right DecisionTaskTimeoutTypeSTART_TO_CLOSE
    fromText e = fromTextFail $ "Unrecognised DecisionTaskTimeoutType: " <> e

instance Read DecisionTaskTimeoutType where
    readsPrec _ = fromTextRead

instance ToText DecisionTaskTimeoutType where
    toText DecisionTaskTimeoutTypeSTART_TO_CLOSE = "START_TO_CLOSE"

instance Show DecisionTaskTimeoutType where
    show = toTextShow

instance FromJSON DecisionTaskTimeoutType where
    parseJSON = fromTextJSON "DecisionTaskTimeoutType"

instance FromJSON v => FromJSON (HashMap DecisionTaskTimeoutType v) where
    parseJSON = fromTextHashJSON

instance ToJSON DecisionTaskTimeoutType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap DecisionTaskTimeoutType v) where
    toJSON = toTextHashJSON

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.

data ContinueAsNewWorkflowExecutionFailedCause
    = ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | ContinueAsNewWorkflowExecutionFailedCauseUNHANDLED_DECISION
    | ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED
    | ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Ord, Generic)

instance Hashable ContinueAsNewWorkflowExecutionFailedCause

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    fromText "DEFAULT_CHILD_POLICY_UNDEFINED" = Right ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED
    fromText "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" = Right ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "DEFAULT_TASK_LIST_UNDEFINED" = Right ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED
    fromText "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" = Right ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    fromText "OPERATION_NOT_PERMITTED" = Right ContinueAsNewWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "UNHANDLED_DECISION" = Right ContinueAsNewWorkflowExecutionFailedCauseUNHANDLED_DECISION
    fromText "WORKFLOW_TYPE_DEPRECATED" = Right ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED
    fromText "WORKFLOW_TYPE_DOES_NOT_EXIST" = Right ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST
    fromText e = fromTextFail $ "Unrecognised ContinueAsNewWorkflowExecutionFailedCause: " <> e

instance Read ContinueAsNewWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_CHILD_POLICY_UNDEFINED = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_LIST_UNDEFINED = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText ContinueAsNewWorkflowExecutionFailedCauseUNHANDLED_DECISION = "UNHANDLED_DECISION"
    toText ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DEPRECATED = "WORKFLOW_TYPE_DEPRECATED"
    toText ContinueAsNewWorkflowExecutionFailedCauseWORKFLOW_TYPE_DOES_NOT_EXIST = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Show ContinueAsNewWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "ContinueAsNewWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap ContinueAsNewWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON ContinueAsNewWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ContinueAsNewWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.

data CompleteWorkflowExecutionFailedCause
    = CompleteWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | CompleteWorkflowExecutionFailedCauseUNHANDLED_DECISION
      deriving (Eq, Ord, Generic)

instance Hashable CompleteWorkflowExecutionFailedCause

instance FromText CompleteWorkflowExecutionFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right CompleteWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "UNHANDLED_DECISION" = Right CompleteWorkflowExecutionFailedCauseUNHANDLED_DECISION
    fromText e = fromTextFail $ "Unrecognised CompleteWorkflowExecutionFailedCause: " <> e

instance Read CompleteWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText CompleteWorkflowExecutionFailedCause where
    toText CompleteWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText CompleteWorkflowExecutionFailedCauseUNHANDLED_DECISION = "UNHANDLED_DECISION"

instance Show CompleteWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON CompleteWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "CompleteWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap CompleteWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON CompleteWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap CompleteWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | If the execution status is closed then this specifies how the execution was
-- closed: COMPLETED: the execution was successfully completed. CANCELED: the
-- execution was canceled.Cancellation allows the implementation to gracefully
-- clean up before the execution is closed. TERMINATED: the execution was
-- force terminated. FAILED: the execution failed to complete. TIMED_OUT: the
-- execution did not complete in the alloted time and was automatically timed
-- out. CONTINUED_AS_NEW: the execution is logically continued. This means the
-- current execution was completed and a new execution was started to carry on
-- the workflow.

data CloseStatus
    = CloseStatusCANCELED
    | CloseStatusCOMPLETED
    | CloseStatusCONTINUED_AS_NEW
    | CloseStatusFAILED
    | CloseStatusTERMINATED
    | CloseStatusTIMED_OUT
      deriving (Eq, Ord, Generic)

instance Hashable CloseStatus

instance FromText CloseStatus where
    fromText "CANCELED" = Right CloseStatusCANCELED
    fromText "COMPLETED" = Right CloseStatusCOMPLETED
    fromText "CONTINUED_AS_NEW" = Right CloseStatusCONTINUED_AS_NEW
    fromText "FAILED" = Right CloseStatusFAILED
    fromText "TERMINATED" = Right CloseStatusTERMINATED
    fromText "TIMED_OUT" = Right CloseStatusTIMED_OUT
    fromText e = fromTextFail $ "Unrecognised CloseStatus: " <> e

instance Read CloseStatus where
    readsPrec _ = fromTextRead

instance ToText CloseStatus where
    toText CloseStatusCANCELED = "CANCELED"
    toText CloseStatusCOMPLETED = "COMPLETED"
    toText CloseStatusCONTINUED_AS_NEW = "CONTINUED_AS_NEW"
    toText CloseStatusFAILED = "FAILED"
    toText CloseStatusTERMINATED = "TERMINATED"
    toText CloseStatusTIMED_OUT = "TIMED_OUT"

instance Show CloseStatus where
    show = toTextShow

instance FromJSON CloseStatus where
    parseJSON = fromTextJSON "CloseStatus"

instance FromJSON v => FromJSON (HashMap CloseStatus v) where
    parseJSON = fromTextHashJSON

instance ToJSON CloseStatus where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap CloseStatus v) where
    toJSON = toTextHashJSON

-- | If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by calling
-- the TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow execution
-- using the StartWorkflowExecution action or the StartChildWorkflowExecution
-- Decision. The supported child policies are: TERMINATE: the child executions
-- will be terminated. REQUEST_CANCEL: a request to cancel will be attempted
-- for each child execution by recording a WorkflowExecutionCancelRequested
-- event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event. ABANDON: no action
-- will be taken. The child executions will continue to run.

data ChildPolicy
    = ChildPolicyABANDON
    | ChildPolicyREQUEST_CANCEL
    | ChildPolicyTERMINATE
      deriving (Eq, Ord, Generic)

instance Hashable ChildPolicy

instance FromText ChildPolicy where
    fromText "ABANDON" = Right ChildPolicyABANDON
    fromText "REQUEST_CANCEL" = Right ChildPolicyREQUEST_CANCEL
    fromText "TERMINATE" = Right ChildPolicyTERMINATE
    fromText e = fromTextFail $ "Unrecognised ChildPolicy: " <> e

instance Read ChildPolicy where
    readsPrec _ = fromTextRead

instance ToText ChildPolicy where
    toText ChildPolicyABANDON = "ABANDON"
    toText ChildPolicyREQUEST_CANCEL = "REQUEST_CANCEL"
    toText ChildPolicyTERMINATE = "TERMINATE"

instance Show ChildPolicy where
    show = toTextShow

instance FromJSON ChildPolicy where
    parseJSON = fromTextJSON "ChildPolicy"

instance FromJSON v => FromJSON (HashMap ChildPolicy v) where
    parseJSON = fromTextHashJSON

instance ToJSON ChildPolicy where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ChildPolicy v) where
    toJSON = toTextHashJSON

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.

data CancelWorkflowExecutionFailedCause
    = CancelWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    | CancelWorkflowExecutionFailedCauseUNHANDLED_DECISION
      deriving (Eq, Ord, Generic)

instance Hashable CancelWorkflowExecutionFailedCause

instance FromText CancelWorkflowExecutionFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right CancelWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED
    fromText "UNHANDLED_DECISION" = Right CancelWorkflowExecutionFailedCauseUNHANDLED_DECISION
    fromText e = fromTextFail $ "Unrecognised CancelWorkflowExecutionFailedCause: " <> e

instance Read CancelWorkflowExecutionFailedCause where
    readsPrec _ = fromTextRead

instance ToText CancelWorkflowExecutionFailedCause where
    toText CancelWorkflowExecutionFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText CancelWorkflowExecutionFailedCauseUNHANDLED_DECISION = "UNHANDLED_DECISION"

instance Show CancelWorkflowExecutionFailedCause where
    show = toTextShow

instance FromJSON CancelWorkflowExecutionFailedCause where
    parseJSON = fromTextJSON "CancelWorkflowExecutionFailedCause"

instance FromJSON v => FromJSON (HashMap CancelWorkflowExecutionFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON CancelWorkflowExecutionFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap CancelWorkflowExecutionFailedCause v) where
    toJSON = toTextHashJSON

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.

data CancelTimerFailedCause
    = CancelTimerFailedCauseOPERATION_NOT_PERMITTED
    | CancelTimerFailedCauseTIMER_ID_UNKNOWN
      deriving (Eq, Ord, Generic)

instance Hashable CancelTimerFailedCause

instance FromText CancelTimerFailedCause where
    fromText "OPERATION_NOT_PERMITTED" = Right CancelTimerFailedCauseOPERATION_NOT_PERMITTED
    fromText "TIMER_ID_UNKNOWN" = Right CancelTimerFailedCauseTIMER_ID_UNKNOWN
    fromText e = fromTextFail $ "Unrecognised CancelTimerFailedCause: " <> e

instance Read CancelTimerFailedCause where
    readsPrec _ = fromTextRead

instance ToText CancelTimerFailedCause where
    toText CancelTimerFailedCauseOPERATION_NOT_PERMITTED = "OPERATION_NOT_PERMITTED"
    toText CancelTimerFailedCauseTIMER_ID_UNKNOWN = "TIMER_ID_UNKNOWN"

instance Show CancelTimerFailedCause where
    show = toTextShow

instance FromJSON CancelTimerFailedCause where
    parseJSON = fromTextJSON "CancelTimerFailedCause"

instance FromJSON v => FromJSON (HashMap CancelTimerFailedCause v) where
    parseJSON = fromTextHashJSON

instance ToJSON CancelTimerFailedCause where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap CancelTimerFailedCause v) where
    toJSON = toTextHashJSON

-- | The type of the timeout that caused this event.

data ActivityTaskTimeoutType
    = ActivityTaskTimeoutTypeHEARTBEAT
    | ActivityTaskTimeoutTypeSCHEDULE_TO_CLOSE
    | ActivityTaskTimeoutTypeSCHEDULE_TO_START
    | ActivityTaskTimeoutTypeSTART_TO_CLOSE
      deriving (Eq, Ord, Generic)

instance Hashable ActivityTaskTimeoutType

instance FromText ActivityTaskTimeoutType where
    fromText "HEARTBEAT" = Right ActivityTaskTimeoutTypeHEARTBEAT
    fromText "SCHEDULE_TO_CLOSE" = Right ActivityTaskTimeoutTypeSCHEDULE_TO_CLOSE
    fromText "SCHEDULE_TO_START" = Right ActivityTaskTimeoutTypeSCHEDULE_TO_START
    fromText "START_TO_CLOSE" = Right ActivityTaskTimeoutTypeSTART_TO_CLOSE
    fromText e = fromTextFail $ "Unrecognised ActivityTaskTimeoutType: " <> e

instance Read ActivityTaskTimeoutType where
    readsPrec _ = fromTextRead

instance ToText ActivityTaskTimeoutType where
    toText ActivityTaskTimeoutTypeHEARTBEAT = "HEARTBEAT"
    toText ActivityTaskTimeoutTypeSCHEDULE_TO_CLOSE = "SCHEDULE_TO_CLOSE"
    toText ActivityTaskTimeoutTypeSCHEDULE_TO_START = "SCHEDULE_TO_START"
    toText ActivityTaskTimeoutTypeSTART_TO_CLOSE = "START_TO_CLOSE"

instance Show ActivityTaskTimeoutType where
    show = toTextShow

instance FromJSON ActivityTaskTimeoutType where
    parseJSON = fromTextJSON "ActivityTaskTimeoutType"

instance FromJSON v => FromJSON (HashMap ActivityTaskTimeoutType v) where
    parseJSON = fromTextHashJSON

instance ToJSON ActivityTaskTimeoutType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ActivityTaskTimeoutType v) where
    toJSON = toTextHashJSON
