{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the compiled information from a RequestEnvironmentInfo request.
-- Related Topics RequestEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RetrieveEnvironmentInfo &AuthParams
-- https://elasticbeanstalk.us-east-1.s3.amazonaws.com/environments%2Fa514386a-709f-4888-9683-068c38d744b4%2Flogs%2Fi-92a3ceff%2F278756a8-7d83-4bc1-93db-b1763163705a.log?Expires=1291236023
-- &AuthParams 2010-11-17T20:40:23.210Z tail i-92a3ceff
-- e8e785c9-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo where

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
retrieveEnvironmentInfo :: EnvironmentInfoType
                        -> RetrieveEnvironmentInfo
retrieveEnvironmentInfo p1 = undefined $ RetrieveEnvironmentInfo
    { reinInfoType = p1
    , reinEnvironmentId = Nothing
    , reinEnvironmentName = Nothing
    }

data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo
    { reinEnvironmentId :: Maybe Text
      -- ^ The ID of the data's environment. If no such environment is found, returns
      -- an InvalidParameterValue error. Condition: You must specify either this or
      -- an EnvironmentName, or both. If you do not specify either, AWS Elastic
      -- Beanstalk returns MissingRequiredParameter error.
    , reinEnvironmentName :: Maybe Text
      -- ^ The name of the data's environment. If no such environment is found,
      -- returns an InvalidParameterValue error. Condition: You must specify either
      -- this or an EnvironmentId, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    , reinInfoType :: !EnvironmentInfoType
      -- ^ The type of information to retrieve.
    } deriving (Eq, Show, Generic)

instance ToQuery RetrieveEnvironmentInfo

instance AWSRequest RetrieveEnvironmentInfo where
    type Er RetrieveEnvironmentInfo = ElasticBeanstalkError
    type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse
    request = getQuery service "RetrieveEnvironmentInfo"

data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { reinrsEnvironmentInfo :: [EnvironmentInfoDescription]
      -- ^ The EnvironmentInfoDescription of the environment.
    } deriving (Eq, Show, Generic)

instance FromXML RetrieveEnvironmentInfoResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RetrieveEnvironmentInfoResponse"
        :| ["RetrieveEnvironmentInfoResult"]
