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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeCommunications :: Text
                       -> DescribeCommunications
describeCommunications p1 = DescribeCommunications
    { dcsCaseId = p1
    , dcsAfterTime = Nothing
    , dcsBeforeTime = Nothing
    , dcsMaxResults = Nothing
    , dcsNextToken = Nothing
    }

data DescribeCommunications = DescribeCommunications
    { dcsAfterTime :: Maybe Text
      -- ^ The start date for a filtered date search on support case communications.
    , dcsBeforeTime :: Maybe Text
      -- ^ The end date for a filtered date search on support case communications.
    , dcsCaseId :: !Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    , dcsMaxResults :: Maybe Int
      -- ^ The maximum number of results to return before paginating.
    , dcsNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCommunications where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeCommunications where
    type Er DescribeCommunications = SupportError
    type Rs DescribeCommunications = DescribeCommunicationsResponse
    request  = getJSON service
    response = responseJSON

data DescribeCommunicationsResponse = DescribeCommunicationsResponse
    { dcsrsCommunications :: [Communication]
      -- ^ The communications for the case.
    , dcsrsNextToken :: Maybe Text
      -- ^ A resumption point for pagination.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCommunicationsResponse where
    fromJSON = genericFromJSON jsonOptions

