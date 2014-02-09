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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createKeyPair :: Text
              -- ^ The unique name for the new key pair.
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
    { ckprrKeyFingerprint :: Maybe Text
      -- ^ The SHA-1 digest of the DER encoded private key.
    , ckprrKeyMaterial :: Maybe Text
      -- ^ The unencrypted PEM encoded RSA private key.
    , ckprrKeyName :: Maybe Text
      -- ^ The name of the key pair.
    } deriving (Eq, Show, Generic)

instance FromXML CreateKeyPairResponse where
    fromXMLOptions = xmlOptions
