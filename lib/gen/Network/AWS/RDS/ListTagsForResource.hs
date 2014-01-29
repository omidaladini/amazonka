{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all tags on an Amazon RDS resource. For an overview on tagging an
-- Amazon RDS resource, see Tagging Amazon RDS Resources.
module Network.AWS.RDS.ListTagsForResource where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
listTagsForResource :: Text
                    -> AWS (Either RDSError ListTagsForResourceResponse)
listTagsForResource p1 = undefined $ ListTagsForResource
    { ltfrmResourceName = p1
    }

data ListTagsForResource = ListTagsForResource
    { ltfrmResourceName :: !Text
      -- ^ The Amazon RDS resource with tags to be listed. This value is an Amazon
      -- Resource Name (ARN). For information about creating an ARN, see
      -- Constructing an RDS Amazon Resource Name (ARN).
    } deriving (Eq, Show, Generic)

instance ToQuery ListTagsForResource

instance AWSRequest ListTagsForResource where
    type Er ListTagsForResource = RDSError
    type Rs ListTagsForResource = ListTagsForResourceResponse
    request = getQuery service "ListTagsForResource"

data ListTagsForResourceResponse = ListTagsForResourceResponse
    { ltfrmrsTagList :: [Tag]
      -- ^ List of tags returned by the ListTagsForResource operation.
    } deriving (Eq, Show, Generic)

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListTagsForResourceResponse"
        :| ["ListTagsForResourceResult"]
