{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the refresh status of the Trusted Advisor checks that have the
-- specified check IDs. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses where

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
describeTrustedAdvisorCheckRefreshStatuses :: [Text]
                                           -> AWS (Either SupportError DescribeTrustedAdvisorCheckRefreshStatusesResponse)
describeTrustedAdvisorCheckRefreshStatuses p1 = undefined $ DescribeTrustedAdvisorCheckRefreshStatuses
    { dtacrsrCheckIds = p1
    }

data DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { dtacrsrCheckIds :: [Text]
      -- ^ The IDs of the Trusted Advisor checks.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Er DescribeTrustedAdvisorCheckRefreshStatuses = SupportError
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    request  = getJSON service
    response = responseJSON

data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { dtacrsrrsStatuses :: [TrustedAdvisorCheckRefreshStatus]
      -- ^ The refresh status of the specified Trusted Advisor checks.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse
