{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns communications regarding the support case. You can use the
-- AfterTime and BeforeTime parameters to filter by date. The CaseId parameter
-- enables you to identify a specific case by its CaseId value. The MaxResults
-- and NextToken parameters enable you to control the pagination of the result
-- set. Set MaxResults to the number of cases you want displayed on each page,
-- and use NextToken to specify the resumption of pagination.
module Network.AWS.Support.DescribeCommunications where

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

data DescribeCommunications = DescribeCommunications
    { dcsafterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case communications.
    , dcsbeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case communications.
    , dcscaseId :: !Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    , dcsmaxResults :: Maybe Int
      -- ^ The maximum number of results to return before paginating.
    , dcsnextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCommunications

instance AWSRequest DescribeCommunications where
    type Er DescribeCommunications = SupportError
    type Rs DescribeCommunications = DescribeCommunicationsResponse
    request  = getJSON service
    response = responseJSON

data DescribeCommunicationsResponse = DescribeCommunicationsResponse
    { dcsrscommunications :: [Communication]
      -- ^ The communications for the case.
    , dcsrsnextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCommunicationsResponse
