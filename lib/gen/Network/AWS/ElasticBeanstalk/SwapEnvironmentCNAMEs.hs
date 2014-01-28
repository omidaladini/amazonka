{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Swaps the CNAMEs of two environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?SourceEnvironmentName=SampleApp
-- &DestinationEnvironmentName=SampleApp2 &Operation=SwapEnvironmentCNAMEs
-- &AuthParams f4e1b145-9080-11e0-8e5a-a558e0ce1fc4.
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs where

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

import Network.AWS.ElasticBeanstalk.Service
import Network.AWS.ElasticBeanstalk.Types

data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { secnamemDestinationEnvironmentId :: Maybe Text
      -- ^ The ID of the destination environment. Condition: You must specify at least
      -- the DestinationEnvironmentID or the DestinationEnvironmentName. You may
      -- also specify both. You must specify the SourceEnvironmentId with the
      -- DestinationEnvironmentId.
    , secnamemDestinationEnvironmentName :: Maybe Text
      -- ^ The name of the destination environment. Condition: You must specify at
      -- least the DestinationEnvironmentID or the DestinationEnvironmentName. You
      -- may also specify both. You must specify the SourceEnvironmentName with the
      -- DestinationEnvironmentName.
    , secnamemSourceEnvironmentId :: Maybe Text
      -- ^ The ID of the source environment. Condition: You must specify at least the
      -- SourceEnvironmentID or the SourceEnvironmentName. You may also specify
      -- both. If you specify the SourceEnvironmentId, you must specify the
      -- DestinationEnvironmentId.
    , secnamemSourceEnvironmentName :: Maybe Text
      -- ^ The name of the source environment. Condition: You must specify at least
      -- the SourceEnvironmentID or the SourceEnvironmentName. You may also specify
      -- both. If you specify the SourceEnvironmentName, you must specify the
      -- DestinationEnvironmentName.
    } deriving (Eq, Show, Generic)

instance ToQuery SwapEnvironmentCNAMEs

instance AWSRequest SwapEnvironmentCNAMEs where
    type Er SwapEnvironmentCNAMEs = ElasticBeanstalkError
    type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse
    request = getQuery service "SwapEnvironmentCNAMEs"

data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse
    deriving (Eq, Show, Generic)

instance FromXML SwapEnvironmentCNAMEsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SwapEnvironmentCNAMEsResponse"
        :| ["SwapEnvironmentCNAMEsResult"]
