{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeKeyPairs operation returns information about key pairs
-- available to you. If you specify key pairs, information about those key
-- pairs is returned. Otherwise, information for all registered key pairs is
-- returned.
module Network.AWS.EC2.DescribeKeyPairs where

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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields where applicable.
describeKeyPairs :: AWS (Either EC2Error DescribeKeyPairsResponse)
describeKeyPairs = undefined $ DescribeKeyPairs
    { dkpsDryRun = Nothing
    , dkpsFilters = []
    , dkpsKeyNames = []
    }

data DescribeKeyPairs = DescribeKeyPairs
    { dkpsDryRun :: Maybe Bool
    , dkpsFilters :: [Filter]
      -- ^ A list of filters used to match properties for KeyPairs. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dkpsKeyNames :: [Text]
      -- ^ The optional list of key pair names to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeKeyPairs

instance AWSRequest DescribeKeyPairs where
    type Er DescribeKeyPairs = EC2Error
    type Rs DescribeKeyPairs = DescribeKeyPairsResponse
    request = getQuery service "DescribeKeyPairs"

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { dkpsrsKeyPairs :: [KeyPairInfo]
      -- ^ The list of described key pairs.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeKeyPairsResponse where
    fromXMLOptions = xmlOptions
