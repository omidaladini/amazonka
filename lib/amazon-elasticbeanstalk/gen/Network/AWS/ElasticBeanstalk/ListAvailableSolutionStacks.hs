{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available solution stack names.
-- https://elasticbeanstalk.us-east-1.amazon.com/?Operation=ListAvailableSolutionStacks
-- &AuthParams 64bit Amazon Linux running Tomcat 6 32bit Amazon Linux running
-- Tomcat 6 64bit Amazon Linux running Tomcat 7 32bit Amazon Linux running
-- Tomcat 7 f21e2a92-f1fc-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks

data ListAvailableSolutionStacks = ListAvailableSolutionStacks
    deriving (Eq, Show, Generic)

instance ToQuery ListAvailableSolutionStacks

instance AWSRequest ListAvailableSolutionStacks where
    type Er ListAvailableSolutionStacks = ElasticBeanstalkError
    type Rs ListAvailableSolutionStacks = ListAvailableSolutionStacksResponse
    request = getQuery service "ListAvailableSolutionStacks"

data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse
    { lassrmSolutionStackDetails :: [SolutionStackDescription]
      -- ^ A list of available solution stacks and their SolutionStackDescription.
    , lassrmSolutionStacks :: [Text]
      -- ^ A list of available solution stacks.
    } deriving (Eq, Show, Generic)

instance FromXML ListAvailableSolutionStacksResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListAvailableSolutionStacksResponse"
        :| ["ListAvailableSolutionStacksResult"]
