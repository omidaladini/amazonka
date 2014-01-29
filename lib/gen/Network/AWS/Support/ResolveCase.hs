{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a CaseId and returns the initial state of the case along with the
-- state of the case after the call to ResolveCase completed.
module Network.AWS.Support.ResolveCase where

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
resolveCase :: AWS (Either SupportError ResolveCaseResponse)
resolveCase = undefined $ ResolveCase
    { rcrCaseId = Nothing
    }

data ResolveCase = ResolveCase
    { rcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The case ID is
      -- an alphanumeric string formatted as shown in this example:
      -- case-12345678910-2013-c4c1d2bf33c5cf47.
    } deriving (Eq, Show, Generic)

instance ToJSON ResolveCase

instance AWSRequest ResolveCase where
    type Er ResolveCase = SupportError
    type Rs ResolveCase = ResolveCaseResponse
    request  = getJSON service
    response = responseJSON

data ResolveCaseResponse = ResolveCaseResponse
    { rcrrsFinalCaseStatus :: Maybe Text
      -- ^ The status of the case after the ResolveCase request was processed.
    , rcrrsInitialCaseStatus :: Maybe Text
      -- ^ The status of the case when the ResolveCase request was sent.
    } deriving (Eq, Show, Generic)

instance FromJSON ResolveCaseResponse
