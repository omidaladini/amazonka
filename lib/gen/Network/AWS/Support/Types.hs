{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Support.Types where

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

import Network.AWS.Support.Service

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor
-- DescribeTrustedAdvisorCheckSummaries.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary
    { tarsresourcesFlagged :: !Integer
      -- ^ The number of AWS resources that were flagged (listed) by the Trusted
      -- Advisor check.
    , tarsresourcesIgnored :: !Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because information
      -- was unavailable.
    , tarsresourcesProcessed :: !Integer
      -- ^ The number of AWS resources that were analyzed by the Trusted Advisor
      -- check.
    , tarsresourcesSuppressed :: !Integer
      -- ^ The number of AWS resources ignored by Trusted Advisor because they were
      -- marked as suppressed by the user.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorResourcesSummary
instance ToJSON TrustedAdvisorResourcesSummary

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail
    { tardisSuppressed :: Maybe Bool
      -- ^ Specifies whether the AWS resource was ignored by Trusted Advisor because
      -- it was marked as suppressed by the user.
    , tardmetadata :: [Text]
      -- ^ Additional information about the identified resource. The exact metadata
      -- and its order can be obtained by inspecting the
      -- TrustedAdvisorCheckDescription object returned by the call to
      -- DescribeTrustedAdvisorChecks.
    , tardregion :: !Text
      -- ^ The AWS region in which the identified resource is located.
    , tardresourceId :: !Text
      -- ^ The unique identifier for the identified resource.
    , tardstatus :: !Text
      -- ^ The status code for the resource identified in the Trusted Advisor check.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorResourceDetail
instance ToJSON TrustedAdvisorResourceDetail

-- | The summary information about cost savings for a Trusted Advisor check that
-- is in the Cost Optimizing category.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary
    { tacosestimatedMonthlySavings :: !Double
      -- ^ The estimated monthly savings that might be realized if the recommended
      -- actions are taken.
    , tacosestimatedPercentMonthlySavings :: !Double
      -- ^ The estimated percentage of savings that might be realized if the
      -- recommended actions are taken.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCostOptimizingSummary
instance ToJSON TrustedAdvisorCostOptimizingSummary

-- | A summary of a Trusted Advisor check result, including the alert status,
-- last refresh, and number of resources examined.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary
    { tacscategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check. Cost
      -- Optimizing is the only category that is currently supported.
    , tacscheckId :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , tacshasFlaggedResources :: Maybe Bool
      -- ^ Specifies whether the Trusted Advisor check has flagged resources.
    , tacsresourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to Trusted Advisor
      -- DescribeTrustedAdvisorCheckSummaries.
    , tacsstatus :: !Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow), "error"
      -- (red), or "not_available".
    , tacstimestamp :: !Text
      -- ^ The time of the last refresh of the check.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCheckSummary
instance ToJSON TrustedAdvisorCheckSummary

-- | The detailed results of the Trusted Advisor check.
data TrustedAdvisorCheckResult = TrustedAdvisorCheckResult
    { tacrcategorySpecificSummary :: TrustedAdvisorCategorySpecificSummary
      -- ^ Summary information that relates to the category of the check. Cost
      -- Optimizing is the only category that is currently supported.
    , tacrcheckId :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , tacrflaggedResources :: [TrustedAdvisorResourceDetail]
      -- ^ The details about each resource listed in the check result.
    , tacrresourcesSummary :: TrustedAdvisorResourcesSummary
      -- ^ Details about AWS resources that were analyzed in a call to Trusted Advisor
      -- DescribeTrustedAdvisorCheckSummaries.
    , tacrstatus :: !Text
      -- ^ The alert status of the check: "ok" (green), "warning" (yellow), "error"
      -- (red), or "not_available".
    , tacrtimestamp :: !Text
      -- ^ The time of the last refresh of the check.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCheckResult
instance ToJSON TrustedAdvisorCheckResult

-- | The current refresh status for a check, including the amount of time until
-- the check is eligible for refresh.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus
    { tacrscheckId :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , tacrsmillisUntilNextRefreshable :: !Integer
      -- ^ The amount of time, in milliseconds, until the Trusted Advisor check is
      -- eligible for refresh.
    , tacrsstatus :: !Text
      -- ^ The status of the Trusted Advisor check for which a refresh has been
      -- requested: "none", "enqueued", "processing", "success", or "abandoned".
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCheckRefreshStatus
instance ToJSON TrustedAdvisorCheckRefreshStatus

-- | The description and metadata for a Trusted Advisor check.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription
    { tacdcategory :: !Text
      -- ^ The category of the Trusted Advisor check.
    , tacddescription :: !Text
      -- ^ The description of the Trusted Advisor check, which includes the alert
      -- criteria and recommended actions (contains HTML markup).
    , tacdid :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , tacdmetadata :: [Text]
      -- ^ The column headings for the data returned by the Trusted Advisor check. The
      -- order of the headings corresponds to the order of the data in the Metadata
      -- element of the TrustedAdvisorResourceDetail for the check.
    , tacdname :: !Text
      -- ^ The display name for the Trusted Advisor check.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCheckDescription
instance ToJSON TrustedAdvisorCheckDescription

-- | Summary information that relates to the category of the check. Cost
-- Optimizing is the only category that is currently supported.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary
    { tacsscostOptimizing :: Maybe TrustedAdvisorCostOptimizingSummary
      -- ^ The summary information about cost savings for a Trusted Advisor check that
      -- is in the Cost Optimizing category.
    } deriving (Eq, Show, Generic)

instance FromJSON TrustedAdvisorCategorySpecificSummary
instance ToJSON TrustedAdvisorCategorySpecificSummary

-- | A code and name pair that represent a severity level that can be applied to
-- a support case.
data SeverityLevel = SeverityLevel
    { slcode :: Maybe Text
      -- ^ One of four values: "low," "medium," "high," and "urgent". These values
      -- correspond to response times returned to the caller in SeverityLevel.name.
    , slname :: Maybe Text
      -- ^ The name of the severity level that corresponds to the severity level code.
    } deriving (Eq, Show, Generic)

instance FromJSON SeverityLevel
instance ToJSON SeverityLevel

-- | Represents an AWS Service returned by the DescribeServices action.
data Service = Service
    { scategories :: [Category]
      -- ^ A list of categories that describe the type of support issue a case
      -- describes. Categories consist of a category name and a category code.
      -- Category names and codes are passed to AWS Support when you call
      -- CreateCase.
    , scode :: Maybe Text
      -- ^ The code for an AWS service returned by DescribeServices response. Has a
      -- corresponding name represented by Service.name.
    , sname :: Maybe Text
      -- ^ The friendly name for an AWS service. Has a corresponding code represented
      -- by Service.code.
    } deriving (Eq, Show, Generic)

instance FromJSON Service
instance ToJSON Service

-- | The five most recent communications between you and AWS Support Center.
-- Includes a nextToken to retrieve the next set of communications.
data RecentCaseCommunications = RecentCaseCommunications
    { rcccommunications :: [Communication]
      -- ^ The five most recent communications associated with the case.
    , rccnextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance FromJSON RecentCaseCommunications
instance ToJSON RecentCaseCommunications

-- | Exposes the fields used by a communication for an AWS Support case.
data Communication = Communication
    { dbody :: Maybe Text
      -- ^ The text of the communication between the customer and AWS Support.
    , dcaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    , dsubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the AWS Support case.
    , dtimeCreated :: Maybe Text
      -- ^ The time the support case was created.
    } deriving (Eq, Show, Generic)

instance FromJSON Communication
instance ToJSON Communication

-- | A JSON-formatted name/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices response
-- for each AWS service.
data Category = Category
    { ccode :: Maybe Text
      -- ^ The category code for the support case.
    , cname :: Maybe Text
      -- ^ The category name for the support case.
    } deriving (Eq, Show, Generic)

instance FromJSON Category
instance ToJSON Category

-- | A JSON-formatted object that contains the metadata for a support case. It
-- is contained the response from a DescribeCases request. CaseDetails
-- contains the following fields: CaseID. The AWS Support case ID requested or
-- returned in the call. The case ID is an alphanumeric string formatted as
-- shown in this example: case-12345678910-2013-c4c1d2bf33c5cf47.
-- CategoryCode. The category of problem for the AWS Support case. Corresponds
-- to the CategoryCode values returned by a call to DescribeServices.
-- DisplayId. The identifier for the case on pages in the AWS Support Center.
-- Language. The ISO 639-1 code for the language in which AWS provides
-- support. AWS Support currently supports English ("en") and Japanese ("ja").
-- Language parameters must be passed explicitly for operations that take
-- them. RecentCommunications. One or more Communication objects. Fields of
-- these objects are Body, CaseId, SubmittedBy, and TimeCreated. NextToken. A
-- resumption point for pagination. ServiceCode. The identifier for the AWS
-- service that corresponds to the service code defined in the call to
-- DescribeServices. SeverityCode. The severity code assigned to the case.
-- Contains one of the values returned by the call to DescribeSeverityLevels.
-- Status. The status of the case in the AWS Support Center. Subject. The
-- subject line of the case. SubmittedBy. The email address of the account
-- that submitted the case. TimeCreated. The time the case was created, in
-- ISO-8601 format.
data CaseDetails = CaseDetails
    { cdcaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    , cdcategoryCode :: Maybe Text
      -- ^ The category of problem for the AWS Support case.
    , cdccEmailAddresses :: [Text]
      -- ^ The email addresses that receive copies of communication about the case.
    , cddisplayId :: Maybe Text
      -- ^ The ID displayed for the case in the AWS Support Center. This is a numeric
      -- string.
    , cdlanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    , cdrecentCommunications :: Maybe RecentCaseCommunications
      -- ^ The five most recent communications between you and AWS Support Center.
      -- Includes a nextToken to retrieve the next set of communications.
    , cdserviceCode :: Maybe Text
      -- ^ The code for the AWS service returned by the call to DescribeServices.
    , cdseverityCode :: Maybe Text
      -- ^ The code for the severity level returned by the call to
      -- DescribeSeverityLevels.
    , cdstatus :: Maybe Text
      -- ^ The status of the case.
    , cdsubject :: Maybe Text
      -- ^ The subject line for the case in the AWS Support Center.
    , cdsubmittedBy :: Maybe Text
      -- ^ The email address of the account that submitted the case.
    , cdtimeCreated :: Maybe Text
      -- ^ The time that the case was case created in the AWS Support Center.
    } deriving (Eq, Show, Generic)

instance FromJSON CaseDetails
instance ToJSON CaseDetails
