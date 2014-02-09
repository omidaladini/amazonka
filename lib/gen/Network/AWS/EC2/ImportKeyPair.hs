{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Imports the public key from an RSA key pair created with a third-party
-- tool. This operation differs from CreateKeyPair as the private key is never
-- transferred between the caller and AWS servers. RSA key pairs are easily
-- created on Microsoft Windows and Linux OS systems using the ssh-keygen
-- command line tool provided with the standard OpenSSH installation. Standard
-- library support for RSA key pair creation is also available for Java, Ruby,
-- Python, and many other programming languages. The following formats are
-- supported: OpenSSH public key format, Base64 encoded DER format. SSH public
-- key file format as specified in RFC4716 .
module Network.AWS.EC2.ImportKeyPair where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
importKeyPair :: Text
              -- ^ The unique name for the key pair.
              -> Blob
              -- ^ The public key portion of the key pair being imported.
              -> ImportKeyPair
importKeyPair p1 p2 = ImportKeyPair
    { ikpKeyName = p1
    , ikpPublicKeyMaterial = p2
    , ikpDryRun = Nothing
    }

data ImportKeyPair = ImportKeyPair
    { ikpDryRun :: Maybe Bool
    , ikpKeyName :: !Text
      -- ^ The unique name for the key pair.
    , ikpPublicKeyMaterial :: !Blob
      -- ^ The public key portion of the key pair being imported.
    } deriving (Eq, Show, Generic)

instance ToQuery ImportKeyPair

instance AWSRequest ImportKeyPair where
    type Er ImportKeyPair = EC2Error
    type Rs ImportKeyPair = ImportKeyPairResponse
    request = getQuery service "ImportKeyPair"

data ImportKeyPairResponse = ImportKeyPairResponse
    { ikprKeyFingerprint :: Maybe Text
      -- ^ The MD5 public key fingerprint as specified in section 4 of RFC4716 .
    , ikprKeyName :: Maybe Text
      -- ^ The specified unique key pair name.
    } deriving (Eq, Show, Generic)

instance FromXML ImportKeyPairResponse where
    fromXMLOptions = xmlOptions
