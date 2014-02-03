{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateKeyPair operation creates a new 2048 bit RSA key pair and returns
-- a unique ID that can be used to reference this key pair when launching new
-- instances. For more information, see RunInstances.
module Network.AWS.EC2.CreateKeyPair where

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
createKeyPair :: Text
              -> CreateKeyPair
createKeyPair p1 = CreateKeyPair
    { ckprKeyName = p1
    , ckprDryRun = Nothing
    }

data CreateKeyPair = CreateKeyPair
    { ckprDryRun :: Maybe Bool
    , ckprKeyName :: !Text
      -- ^ The unique name for the new key pair.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateKeyPair

instance AWSRequest CreateKeyPair where
    type Er CreateKeyPair = EC2Error
    type Rs CreateKeyPair = CreateKeyPairResponse
    request = getQuery service "CreateKeyPair"

data CreateKeyPairResponse = CreateKeyPairResponse
    { ckprrsKeyFingerprint :: Maybe Text
      -- ^ The SHA-1 digest of the DER encoded private key.
    , ckprrsKeyMaterial :: Maybe Text
      -- ^ The unencrypted PEM encoded RSA private key.
    , ckprrsKeyName :: Maybe Text
      -- ^ The name of the key pair.
    } deriving (Eq, Show, Generic)

instance FromXML CreateKeyPairResponse where
    fromXMLOptions = xmlOptions
