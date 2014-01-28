{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeServices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current list of AWS services and a list of service categories
-- that applies to each one. You then use service names and categories in your
-- CreateCase requests. Each AWS service has its own set of categories. The
-- service codes and category codes correspond to the values that are
-- displayed in the Service and Category drop-down lists on the AWS Support
-- Center Open a new case page. The values in those fields, however, do not
-- necessarily match the service codes and categories returned by the
-- DescribeServices request. Always use the service codes and categories
-- obtained programmatically. This practice ensures that you always have the
-- most recent set of service and category codes.
module Network.AWS.Support.DescribeServices where

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

data DescribeServices = DescribeServices
    { dsrlanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS
      -- Support currently supports English ("en") and Japanese ("ja"). Language
      -- parameters must be passed explicitly for operations that take them.
    , dsrserviceCodeList :: [Text]
      -- ^ A JSON-formatted list of service codes available for AWS services.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeServices

instance AWSRequest DescribeServices where
    type Er DescribeServices = SupportError
    type Rs DescribeServices = DescribeServicesResponse
    request  = getJSON service
    response = responseJSON

data DescribeServicesResponse = DescribeServicesResponse
    { dsrrsservices :: [Service]
      -- ^ A JSON-formatted list of AWS services.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeServicesResponse
