{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates a request to compile the specified type of information of the
-- deployed environment. Setting the InfoType to tail compiles the last lines
-- from the application server log files of every Amazon EC2 instance in your
-- environment. Use RetrieveEnvironmentInfo to access the compiled
-- information. Related Topics RetrieveEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RequestEnvironmentInfo &AuthParams
-- 126a4ff3-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo where

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

-- | Convenience method utilising default fields where applicable.
requestEnvironmentInfo :: EnvironmentInfoType
                       -> AWS (Either ElasticBeanstalkError RequestEnvironmentInfoResponse)
requestEnvironmentInfo p1 = undefined $ RequestEnvironmentInfo
    { reimInfoType = p1
    , reimEnvironmentId = Nothing
    , reimEnvironmentName = Nothing
    }

data RequestEnvironmentInfo = RequestEnvironmentInfo
    { reimEnvironmentId :: Maybe Text
      -- ^ The ID of the environment of the requested data. If no such environment is
      -- found, RequestEnvironmentInfo returns an InvalidParameterValue error.
      -- Condition: You must specify either this or an EnvironmentName, or both. If
      -- you do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , reimEnvironmentName :: Maybe Text
      -- ^ The name of the environment of the requested data. If no such environment
      -- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
      -- Condition: You must specify either this or an EnvironmentId, or both. If
      -- you do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , reimInfoType :: !EnvironmentInfoType
      -- ^ The type of information to request.
    } deriving (Eq, Show, Generic)

instance ToQuery RequestEnvironmentInfo

instance AWSRequest RequestEnvironmentInfo where
    type Er RequestEnvironmentInfo = ElasticBeanstalkError
    type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse
    request = getQuery service "RequestEnvironmentInfo"

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse
    deriving (Eq, Show, Generic)

instance FromXML RequestEnvironmentInfoResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RequestEnvironmentInfoResponse"
        :| ["RequestEnvironmentInfoResult"]
