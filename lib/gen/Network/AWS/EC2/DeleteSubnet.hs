{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a subnet from a VPC. You must terminate all running instances in
-- the subnet before deleting it, otherwise Amazon VPC returns an error.
module Network.AWS.EC2.DeleteSubnet where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSubnet :: Text
             -> DeleteSubnet
deleteSubnet p1 = undefined $ DeleteSubnet
    { dstSubnetId = p1
    , dstDryRun = Nothing
    }

data DeleteSubnet = DeleteSubnet
    { dstDryRun :: Maybe Bool
    , dstSubnetId :: !Text
      -- ^ The ID of the subnet you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSubnet

instance AWSRequest DeleteSubnet where
    type Er DeleteSubnet = EC2Error
    type Rs DeleteSubnet = DeleteSubnetResponse
    request = getQuery service "DeleteSubnet"

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSubnetResponse where
    fromXMLOptions = xmlOptions
