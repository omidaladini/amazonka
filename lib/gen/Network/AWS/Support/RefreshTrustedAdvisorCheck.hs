{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a refresh of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks. The response contains a
-- RefreshTrustedAdvisorCheckResult object, which contains these fields:
-- Status. The refresh status of the check: "none", "enqueued", "processing",
-- "success", or "abandoned". MillisUntilNextRefreshable. The amount of time,
-- in milliseconds, until the check is eligible for refresh. CheckId. The
-- unique identifier for the check.
module Network.AWS.Support.RefreshTrustedAdvisorCheck where

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
refreshTrustedAdvisorCheck :: Text
                           -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck p1 = undefined $ RefreshTrustedAdvisorCheck
    { rtacrCheckId = p1
    }

data RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck
    { rtacrCheckId :: !Text
      -- ^ The unique identifier for the Trusted Advisor check.
    } deriving (Eq, Show, Generic)

instance ToJSON RefreshTrustedAdvisorCheck

instance AWSRequest RefreshTrustedAdvisorCheck where
    type Er RefreshTrustedAdvisorCheck = SupportError
    type Rs RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheckResponse
    request  = getJSON service
    response = responseJSON

data RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse
    { rtacrrsStatus :: TrustedAdvisorCheckRefreshStatus
      -- ^ The current refresh status for a check, including the amount of time until
      -- the check is eligible for refresh.
    } deriving (Eq, Show, Generic)

instance FromJSON RefreshTrustedAdvisorCheckResponse
