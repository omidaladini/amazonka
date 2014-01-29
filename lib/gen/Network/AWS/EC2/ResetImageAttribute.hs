{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ResetImageAttribute operation resets an attribute of an AMI to its
-- default value. The productCodes attribute cannot be reset.
module Network.AWS.EC2.ResetImageAttribute where

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
resetImageAttribute :: ResetImageAttributeName
                    -> Text
                    -> AWS (Either EC2Error ResetImageAttributeResponse)
resetImageAttribute p1 p2 = undefined $ ResetImageAttribute
    { riasAttribute = p1
    , riasImageId = p2
    , riasDryRun = Nothing
    }

data ResetImageAttribute = ResetImageAttribute
    { riasAttribute :: !ResetImageAttributeName
      -- ^ The name of the attribute being reset. Available attribute names:
      -- launchPermission.
    , riasDryRun :: Maybe Bool
    , riasImageId :: !Text
      -- ^ The ID of the AMI whose attribute is being reset.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetImageAttribute

instance AWSRequest ResetImageAttribute where
    type Er ResetImageAttribute = EC2Error
    type Rs ResetImageAttribute = ResetImageAttributeResponse
    request = getQuery service "ResetImageAttribute"

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ResetImageAttributeResponse where
    fromXMLOptions = xmlOptions
