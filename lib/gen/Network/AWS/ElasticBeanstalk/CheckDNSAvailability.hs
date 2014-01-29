{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Checks if the specified CNAME is available.
-- https://elasticbeanstalk.us-east-1.amazon.com/?CNAMEPrefix=sampleapplication
-- &Operation=CheckDNSAvailability &AuthParams
-- sampleapplication.elasticbeanstalk.amazonaws.com true
-- 12f6701f-f1d6-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability where

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
checkDNSAvailability :: Text
                     -> AWS (Either ElasticBeanstalkError CheckDNSAvailabilityResponse)
checkDNSAvailability p1 = undefined $ CheckDNSAvailability
    { cdnsamCNAMEPrefix = p1
    }

data CheckDNSAvailability = CheckDNSAvailability
    { cdnsamCNAMEPrefix :: !Text
      -- ^ The prefix used when this CNAME is reserved.
    } deriving (Eq, Show, Generic)

instance ToQuery CheckDNSAvailability

instance AWSRequest CheckDNSAvailability where
    type Er CheckDNSAvailability = ElasticBeanstalkError
    type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse
    request = getQuery service "CheckDNSAvailability"

data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse
    { cdnsamrsAvailable :: Maybe Bool
      -- ^ Indicates if the specified CNAME is available: true : The CNAME is
      -- available. true : The CNAME is not available. true : The CNAME is
      -- available. false : The CNAME is not available.
    , cdnsamrsFullyQualifiedCNAME :: Maybe Text
      -- ^ The fully qualified CNAME to reserve when CreateEnvironment is called with
      -- the provided prefix.
    } deriving (Eq, Show, Generic)

instance FromXML CheckDNSAvailabilityResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CheckDNSAvailabilityResponse"
        :| ["CheckDNSAvailabilityResult"]
