{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey where

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

data RotateEncryptionKey = RotateEncryptionKey
    { rekmClusterIdentifier :: !Text
      -- ^ The unique identifier of the cluster that you want to rotate the encryption
      -- keys for. Constraints: Must be the name of valid cluster that has
      -- encryption enabled.
    } deriving (Eq, Show, Generic)

instance ToQuery RotateEncryptionKey

instance AWSRequest RotateEncryptionKey where
    type Er RotateEncryptionKey = RedshiftError
    type Rs RotateEncryptionKey = RotateEncryptionKeyResponse
    request = getQuery service "RotateEncryptionKey"

data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { rekmrsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Eq, Show, Generic)

instance FromXML RotateEncryptionKeyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RotateEncryptionKeyResponse"
        :| ["RotateEncryptionKeyResult"]
