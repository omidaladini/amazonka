{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified cluster subnet group.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSubnetGroup
-- &ClusterSubnetGroupName=my-subnet-group-2 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T154635Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 3a63806b-6af4-11e2-b27b-4d850b1c672d.
module Network.AWS.Redshift.DeleteClusterSubnetGroup where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields where applicable.
deleteClusterSubnetGroup :: Text
                         -> AWS (Either RedshiftError DeleteClusterSubnetGroupResponse)
deleteClusterSubnetGroup p1 = undefined $ DeleteClusterSubnetGroup
    { dcsgmClusterSubnetGroupName = p1
    }

data DeleteClusterSubnetGroup = DeleteClusterSubnetGroup
    { dcsgmClusterSubnetGroupName :: !Text
      -- ^ The name of the cluster subnet group name to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteClusterSubnetGroup

instance AWSRequest DeleteClusterSubnetGroup where
    type Er DeleteClusterSubnetGroup = RedshiftError
    type Rs DeleteClusterSubnetGroup = DeleteClusterSubnetGroupResponse
    request = getQuery service "DeleteClusterSubnetGroup"

data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteClusterSubnetGroupResponse"
        :| ["DeleteClusterSubnetGroupResult"]
