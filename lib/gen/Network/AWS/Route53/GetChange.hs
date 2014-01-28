{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action returns the current status of a change batch request. The
-- status is one of the following values: - PENDING indicates that the changes
-- in this request have not replicated to all Route 53 DNS servers. This is
-- the initial status of all change batch requests. - INSYNC indicates that
-- the changes have replicated to all Amazon Route 53 DNS servers.
module Network.AWS.Route53.GetChange where

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

import Network.AWS.Route53.Service
import Network.AWS.Route53.Types

data GetChange = GetChange
    { gcrId :: !Text
      -- ^ The ID of the change batch request. The value that you specify here is the
      -- value that ChangeResourceRecordSets returned in the Id element when you
      -- submitted the request.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetChange

instance ToPath GetChange where
    toPath GetChange{..} = Text.concat
        [ "/2012-12-12/change/"
        , toText gcrId
        ]

instance ToQuery GetChange where
    toQuery = const mempty

instance ToXML GetChange where
    toXMLOptions = xmlOptions

instance AWSRequest GetChange where
    type Er GetChange = Route53Error
    type Rs GetChange = GetChangeResponse
    request = getRestXML service

data GetChangeResponse = GetChangeResponse
    { gcrrsChangeInfo :: ChangeInfo
      -- ^ A complex type that contains information about the specified change batch,
      -- including the change batch ID, the status of the change, and the date and
      -- time of the request.
    } deriving (Eq, Show, Generic)

instance FromXML GetChangeResponse where
    fromXMLOptions = xmlOptions
