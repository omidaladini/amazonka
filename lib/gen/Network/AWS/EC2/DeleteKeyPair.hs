{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteKeyPair operation deletes a key pair.
module Network.AWS.EC2.DeleteKeyPair where

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
deleteKeyPair :: Text
              -> AWS (Either EC2Error DeleteKeyPairResponse)
deleteKeyPair p1 = undefined $ DeleteKeyPair
    { dkprKeyName = p1
    , dkprDryRun = Nothing
    }

data DeleteKeyPair = DeleteKeyPair
    { dkprDryRun :: Maybe Bool
    , dkprKeyName :: !Text
      -- ^ The name of the Amazon EC2 key pair to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteKeyPair

instance AWSRequest DeleteKeyPair where
    type Er DeleteKeyPair = EC2Error
    type Rs DeleteKeyPair = DeleteKeyPairResponse
    request = getQuery service "DeleteKeyPair"

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteKeyPairResponse where
    fromXMLOptions = xmlOptions
