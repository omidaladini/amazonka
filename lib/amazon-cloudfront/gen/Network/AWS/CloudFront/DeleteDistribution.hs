{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a distribution.
module Network.AWS.CloudFront.DeleteDistribution where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.CloudFront.Service
import Network.AWS.CloudFront.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteDistribution :: Text
                   -> Text
                   -> DeleteDistribution
deleteDistribution p1 p2 = DeleteDistribution
    { ddrId = p1
    , ddrIfMatch = p2
    }

data DeleteDistribution = DeleteDistribution
    { ddrId :: !Text
      -- ^ The distribution id.
    , ddrIfMatch :: !Text
      -- ^ The value of the ETag header you received when you disabled the
      -- distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteDistribution where
    toHeaders DeleteDistribution{..} =
        [ "If-Match" =: ddrIfMatch
        ]

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText ddrId
        ]

instance ToQuery DeleteDistribution where
    toQuery = const mempty

instance ToXML DeleteDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest DeleteDistribution where
    type Er DeleteDistribution = CloudFrontError
    type Rs DeleteDistribution = DeleteDistributionResponse
    request = deleteRestXML service

data DeleteDistributionResponse = DeleteDistributionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteDistributionResponse where
    fromXMLOptions = xmlOptions
