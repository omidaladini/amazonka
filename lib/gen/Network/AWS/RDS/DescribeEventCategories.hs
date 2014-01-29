{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of categories for all event source types, or, if specified,
-- for a specified source type. You can see a list of the event categories and
-- source types in the Events topic in the Amazon RDS User Guide.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventCategories
-- &SourceType=db-instance &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T013452Z &AWSAccessKeyId=
-- &Signature= db-instance failover low storage maintenance recovery
-- restoration deletion configuration change failover availability creation
-- backup notification ea3bf54b-68ea-11e2-bd13-a92da73b3119.
module Network.AWS.RDS.DescribeEventCategories where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
describeEventCategories :: AWS (Either RDSError DescribeEventCategoriesResponse)
describeEventCategories = undefined $ DescribeEventCategories
    { decmSourceType = Nothing
    }

data DescribeEventCategories = DescribeEventCategories
    { decmSourceType :: Maybe Text
      -- ^ The type of source that will be generating the events. Valid values:
      -- db-instance | db-parameter-group | db-security-group | db-snapshot.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEventCategories

instance AWSRequest DescribeEventCategories where
    type Er DescribeEventCategories = RDSError
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse
    request = getQuery service "DescribeEventCategories"

data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { decmrsEventCategoriesMapList :: [EventCategoriesMap]
      -- ^ A list of EventCategoriesMap data types.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventCategoriesResponse"
        :| ["DescribeEventCategoriesResult"]
