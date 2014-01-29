{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the results of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks. The response contains a
-- TrustedAdvisorCheckResult object, which contains these three objects:
-- TrustedAdvisorCategorySpecificSummary TrustedAdvisorResourceDetail
-- TrustedAdvisorResourcesSummary In addition, the response contains these
-- fields: Status. The alert status of the check: "ok" (green), "warning"
-- (yellow), "error" (red), or "not_available". Timestamp. The time of the
-- last refresh of the check. CheckId. The unique identifier for the check.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult where

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

-- | Convenience method utilising default fields where applicable.
describeTrustedAdvisorCheckResult :: Text
                                  -> AWS (Either SupportError DescribeTrustedAdvisorCheckResultResponse)
describeTrustedAdvisorCheckResult p1 = undefined $ DescribeTrustedAdvisorCheckResult
    { dtacrrCheckId = p1
    , dtacrrLanguage = Nothing
    }

data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult
    { dtacrrCheckId :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    , dtacrrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTrustedAdvisorCheckResult

instance AWSRequest DescribeTrustedAdvisorCheckResult where
    type Er DescribeTrustedAdvisorCheckResult = SupportError
    type Rs DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResultResponse
    request  = getJSON service
    response = responseJSON

data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse
    { dtacrrrsResult :: Maybe TrustedAdvisorCheckResult
      -- ^ The detailed results of the Trusted Advisor check.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTrustedAdvisorCheckResultResponse
