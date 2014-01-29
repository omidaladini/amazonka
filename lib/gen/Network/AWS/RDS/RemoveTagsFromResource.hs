{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes metadata tags from an Amazon RDS resource. For an overview on
-- tagging an Amazon RDS resource, see Tagging Amazon RDS Resources.
module Network.AWS.RDS.RemoveTagsFromResource where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
removeTagsFromResource :: Text
                       -> [Text]
                       -> RemoveTagsFromResource
removeTagsFromResource p1 p2 = undefined $ RemoveTagsFromResource
    { rtfrmResourceName = p1
    , rtfrmTagKeys = p2
    }

data RemoveTagsFromResource = RemoveTagsFromResource
    { rtfrmResourceName :: !Text
      -- ^ The Amazon RDS resource the tags will be removed from. This value is an
      -- Amazon Resource Name (ARN). For information about creating an ARN, see
      -- Constructing an RDS Amazon Resource Name (ARN).
    , rtfrmTagKeys :: [Text]
      -- ^ The tag key (name) of the tag to be removed.
    } deriving (Eq, Show, Generic)

instance ToQuery RemoveTagsFromResource

instance AWSRequest RemoveTagsFromResource where
    type Er RemoveTagsFromResource = RDSError
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
    request = getQuery service "RemoveTagsFromResource"

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    deriving (Eq, Show, Generic)

instance FromXML RemoveTagsFromResourceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RemoveTagsFromResourceResponse"
        :| ["RemoveTagsFromResourceResult"]
