{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source types,
-- go to Amazon Redshift Event Notifications.
module Network.AWS.Redshift.DescribeEventCategories where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeEventCategories :: DescribeEventCategories
describeEventCategories = DescribeEventCategories
    { decmSourceType = Nothing
    }

data DescribeEventCategories = DescribeEventCategories
    { decmSourceType :: Maybe Text
      -- ^ The source type, such as cluster or parameter group, to which the described
      -- event categories apply. Valid values: cluster, snapshot, parameter group,
      -- and security group.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEventCategories

instance AWSRequest DescribeEventCategories where
    type Er DescribeEventCategories = RedshiftError
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse
    request = getQuery service "DescribeEventCategories"

data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { decmrsEventCategoriesMapList :: [EventCategoriesMap]
      -- ^ A list of event categories descriptions.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventCategoriesResponse"
        :| ["DescribeEventCategoriesResult"]
