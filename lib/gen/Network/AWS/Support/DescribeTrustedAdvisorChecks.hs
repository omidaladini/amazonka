{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about all available Trusted Advisor checks, including
-- name, ID, category, description, and metadata. You must specify a language
-- code; English ("en") and Japanese ("ja") are currently supported. The
-- response contains a TrustedAdvisorCheckDescription for each check.
module Network.AWS.Support.DescribeTrustedAdvisorChecks where

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
describeTrustedAdvisorChecks :: Text
                             -> AWS (Either SupportError DescribeTrustedAdvisorChecksResponse)
describeTrustedAdvisorChecks p1 = undefined $ DescribeTrustedAdvisorChecks
    { dtacrLanguage = p1
    }

data DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks
    { dtacrLanguage :: !Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTrustedAdvisorChecks

instance AWSRequest DescribeTrustedAdvisorChecks where
    type Er DescribeTrustedAdvisorChecks = SupportError
    type Rs DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecksResponse
    request  = getJSON service
    response = responseJSON

data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse
    { dtacrrsChecks :: [TrustedAdvisorCheckDescription]
      -- ^ Information about all available Trusted Advisor checks.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTrustedAdvisorChecksResponse
