{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of cases that you specify by passing one or more case IDs.
-- In addition, you can filter the cases by date by setting values for the
-- AfterTime and BeforeTime request parameters. The response returns the
-- following in JSON format: One or more CaseDetails data types. One or more
-- NextToken values, which specify where to paginate the returned records
-- represented by the CaseDetails objects.
module Network.AWS.Support.DescribeCases where

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

data DescribeCases = DescribeCases
    { dcrafterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case communications.
    , dcrbeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case communications.
    , dcrcaseIdList :: [Text]
      -- ^ A list of ID numbers of the support cases you want returned. The maximum
      -- number of cases is 100.
    , dcrdisplayId :: Maybe Text
      -- ^ The ID displayed for a case in the AWS Support Center user interface.
    , dcrincludeResolvedCases :: Maybe Bool
      -- ^ Specifies whether resolved support cases should be included in the
      -- DescribeCases results.
    , dcrlanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    , dcrmaxResults :: Maybe Int
      -- ^ The maximum number of results to return before paginating.
    , dcrnextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCases

instance AWSRequest DescribeCases where
    type Er DescribeCases = SupportError
    type Rs DescribeCases = DescribeCasesResponse
    request  = getJSON service
    response = responseJSON

data DescribeCasesResponse = DescribeCasesResponse
    { dcrrscases :: [CaseDetails]
      -- ^ The details for the cases that match the request.
    , dcrrsnextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCasesResponse
