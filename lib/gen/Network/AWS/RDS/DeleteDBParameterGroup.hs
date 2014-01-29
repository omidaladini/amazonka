{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified DBParameterGroup. The DBParameterGroup cannot be
-- associated with any RDS instances to be deleted. The specified DB parameter
-- group cannot be associated with any DB instances.
-- https://rds.amazonaws.com/ ?Action=DeleteDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A47%3A08.851Z &AWSAccessKeyId= &Signature=
-- 4dc38be9-bf3b-11de-a88b-7b5b3d23b3a7.
module Network.AWS.RDS.DeleteDBParameterGroup where

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
deleteDBParameterGroup :: Text
                       -> AWS (Either RDSError DeleteDBParameterGroupResponse)
deleteDBParameterGroup p1 = undefined $ DeleteDBParameterGroup
    { ddbpgnDBParameterGroupName = p1
    }

data DeleteDBParameterGroup = DeleteDBParameterGroup
    { ddbpgnDBParameterGroupName :: !Text
      -- ^ The name of the DB parameter group. Constraints: Must be the name of an
      -- existing DB parameter group You cannot delete a default DB parameter group
      -- Cannot be associated with any DB instances.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteDBParameterGroup

instance AWSRequest DeleteDBParameterGroup where
    type Er DeleteDBParameterGroup = RDSError
    type Rs DeleteDBParameterGroup = DeleteDBParameterGroupResponse
    request = getQuery service "DeleteDBParameterGroup"

data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteDBParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteDBParameterGroupResponse"
        :| ["DeleteDBParameterGroupResult"]
