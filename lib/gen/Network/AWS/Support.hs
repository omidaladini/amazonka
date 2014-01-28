-- Module      : Network.AWS.Support
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Support
    (
    -- * Operations
    -- ** RefreshTrustedAdvisorCheck
      module Network.AWS.Support.RefreshTrustedAdvisorCheck
    -- ** DescribeCases
    , module Network.AWS.Support.DescribeCases
    -- ** DescribeTrustedAdvisorCheckRefreshStatuses
    , module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
    -- ** DescribeTrustedAdvisorCheckSummaries
    , module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
    -- ** CreateCase
    , module Network.AWS.Support.CreateCase
    -- ** ResolveCase
    , module Network.AWS.Support.ResolveCase
    -- ** DescribeSeverityLevels
    , module Network.AWS.Support.DescribeSeverityLevels
    -- ** DescribeTrustedAdvisorChecks
    , module Network.AWS.Support.DescribeTrustedAdvisorChecks
    -- ** DescribeTrustedAdvisorCheckResult
    , module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
    -- ** DescribeServices
    , module Network.AWS.Support.DescribeServices
    -- ** DescribeCommunications
    , module Network.AWS.Support.DescribeCommunications
    -- ** AddCommunicationToCase
    , module Network.AWS.Support.AddCommunicationToCase

    -- * Types
    -- ** TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary (..)
    -- ** TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail (..)
    -- ** TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary (..)
    -- ** TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary (..)
    -- ** TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult (..)
    -- ** TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus (..)
    -- ** TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription (..)
    -- ** TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary (..)
    -- ** SeverityLevel
    , SeverityLevel (..)
    -- ** Service
    , Service (..)
    -- ** RecentCaseCommunications
    , RecentCaseCommunications (..)
    -- ** Communication
    , Communication (..)
    -- ** Category
    , Category (..)
    -- ** CaseDetails
    , CaseDetails (..)

    -- * Errors
    , SupportError (..)
    ) where

import Network.AWS.Support.Service
import Network.AWS.Support.Types

import Network.AWS.Support.RefreshTrustedAdvisorCheck
import Network.AWS.Support.DescribeCases
import Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
import Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
import Network.AWS.Support.CreateCase
import Network.AWS.Support.ResolveCase
import Network.AWS.Support.DescribeSeverityLevels
import Network.AWS.Support.DescribeTrustedAdvisorChecks
import Network.AWS.Support.DescribeTrustedAdvisorCheckResult
import Network.AWS.Support.DescribeServices
import Network.AWS.Support.DescribeCommunications
import Network.AWS.Support.AddCommunicationToCase
