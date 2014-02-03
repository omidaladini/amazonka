{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.ListTables
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns an array of all the tables associated with the current account and
-- endpoint. List Tables This example requests a list of tables, starting with
-- a table named comp2 and ending after three table names have been returned.
-- { "LastEvaluatedTableName": "Thread", "TableNames":
-- ["Forum","Reply","Thread"] }.
module Network.AWS.DynamoDB.ListTables where

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

import Network.AWS.DynamoDB.Service
import Network.AWS.DynamoDB.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listTables :: ListTables
listTables = ListTables
    { ltiExclusiveStartTableName = Nothing
    , ltiLimit = Nothing
    }

data ListTables = ListTables
    { ltiExclusiveStartTableName :: Maybe Text
      -- ^ The name of the table that starts the list. If you already ran a ListTables
      -- operation and received a LastEvaluatedTableName value in the response, use
      -- that value here to continue the list.
    , ltiLimit :: Maybe Int
      -- ^ A maximum number of table names to return.
    } deriving (Eq, Show, Generic)

instance ToJSON ListTables where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ListTables where
    type Er ListTables = DynamoDBError
    type Rs ListTables = ListTablesResponse
    request  = getJSON service
    response = responseJSON

data ListTablesResponse = ListTablesResponse
    { ltirsLastEvaluatedTableName :: Maybe Text
      -- ^ The name of the last table in the current list, only if some tables for the
      -- account and endpoint have not been returned. This value does not exist in a
      -- response if all table names are already returned. Use this value as the
      -- ExclusiveStartTableName in a new request to continue the list until all the
      -- table names are returned.
    , ltirsTableNames :: [Text]
      -- ^ The names of the tables associated with the current account at the current
      -- endpoint.
    } deriving (Eq, Show, Generic)

instance FromJSON ListTablesResponse where
    fromJSON = genericFromJSON jsonOptions

