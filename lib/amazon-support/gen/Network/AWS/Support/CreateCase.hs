{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.CreateCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new case in the AWS Support Center. This operation is modeled on
-- the behavior of the AWS Support Center Open a new case page. Its parameters
-- require you to specify the following information: ServiceCode. The code for
-- an AWS service. You obtain the ServiceCode by calling DescribeServices.
-- CategoryCode. The category for the service defined for the ServiceCode
-- value. You also obtain the category code for a service by calling
-- DescribeServices. Each AWS service defines its own set of category codes.
-- SeverityCode. A value that indicates the urgency of the case, which in turn
-- determines the response time according to your service level agreement with
-- AWS Support. You obtain the SeverityCode by calling DescribeSeverityLevels.
-- Subject. The Subject field on the AWS Support Center Open a new case page.
-- CommunicationBody. The Description field on the AWS Support Center Open a
-- new case page. Language. The human language in which AWS Support handles
-- the case. English and Japanese are currently supported. CcEmailAddresses.
-- The AWS Support Center CC field on the Open a new case page. You can list
-- email addresses to be copied on any correspondence about the case. The
-- account that opens the case is already identified by passing the AWS
-- Credentials in the HTTP POST method or in a method or function call from
-- one of the programming languages supported by an AWS SDK. IssueType. The
-- type of issue for the case. You can specify either "customer-service" or
-- "technical." If you do not indicate a value, the default is "technical."
-- The AWS Support API does not currently support the ability to add
-- attachments to cases. You can, however, call AddCommunicationToCase to add
-- information to an open case. A successful CreateCase request returns an AWS
-- Support case number. Case numbers are used by the DescribeCases action to
-- retrieve existing AWS Support cases.
module Network.AWS.Support.CreateCase where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.Support.Service
import Network.AWS.Support.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createCase :: Text
           -> Text
           -> CreateCase
createCase p1 p2 = CreateCase
    { ccrCommunicationBody = p1
    , ccrSubject = p2
    , ccrCategoryCode = Nothing
    , ccrCcEmailAddresses = []
    , ccrIssueType = Nothing
    , ccrLanguage = Nothing
    , ccrServiceCode = Nothing
    , ccrSeverityCode = Nothing
    }

data CreateCase = CreateCase
    { ccrCategoryCode :: Maybe Text
      -- ^ The category of problem for the AWS Support case.
    , ccrCcEmailAddresses :: [Text]
      -- ^ A list of email addresses that AWS Support copies on case correspondence.
    , ccrCommunicationBody :: !Text
      -- ^ The communication body text when you create an AWS Support case by calling
      -- CreateCase.
    , ccrIssueType :: Maybe Text
      -- ^ The type of issue for the case. You can specify either "customer-service"
      -- or "technical." If you do not indicate a value, the default is
      -- "technical.".
    , ccrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    , ccrServiceCode :: Maybe Text
      -- ^ The code for the AWS service returned by the call to DescribeServices.
    , ccrSeverityCode :: Maybe Text
      -- ^ The code for the severity level returned by the call to
      -- DescribeSeverityLevels. The availability of severity levels depends on each
      -- customer's support subscription. In other words, your subscription may not
      -- necessarily require the urgent level of response time.
    , ccrSubject :: !Text
      -- ^ The title of the AWS Support case.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateCase where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateCase where
    type Er CreateCase = SupportError
    type Rs CreateCase = CreateCaseResponse
    request  = getJSON service
    response = responseJSON

data CreateCaseResponse = CreateCaseResponse
    { ccrrsCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateCaseResponse where
    fromJSON = genericFromJSON jsonOptions

