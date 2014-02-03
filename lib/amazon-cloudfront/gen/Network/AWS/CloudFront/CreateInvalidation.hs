{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new invalidation.
module Network.AWS.CloudFront.CreateInvalidation where

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
createInvalidation :: Text
                   -> InvalidationBatch
                   -> CreateInvalidation
createInvalidation p1 p2 = CreateInvalidation
    { cirDistributionId = p1
    , cirInvalidationBatch = p2
    }

data CreateInvalidation = CreateInvalidation
    { cirDistributionId :: !Text
      -- ^ The distribution's id.
    , cirInvalidationBatch :: InvalidationBatch
      -- ^ The batch information for the invalidation.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateInvalidation

instance ToPath CreateInvalidation where
    toPath CreateInvalidation{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText cirDistributionId
        , "/invalidation"
        ]

instance ToQuery CreateInvalidation where
    toQuery = const mempty

instance ToXML CreateInvalidation where
    toXMLOptions = xmlOptions

instance AWSRequest CreateInvalidation where
    type Er CreateInvalidation = CloudFrontError
    type Rs CreateInvalidation = CreateInvalidationResponse
    request = postRestXML service

data CreateInvalidationResponse = CreateInvalidationResponse
    { cirrsInvalidation :: Maybe Invalidation
      -- ^ The invalidation's information.
    , cirrsLocation :: Maybe Text
      -- ^ The fully qualified URI of the distribution and invalidation batch request,
      -- including the Invalidation ID.
    } deriving (Eq, Show, Generic)

instance FromXML CreateInvalidationResponse where
    fromXMLOptions = xmlOptions
