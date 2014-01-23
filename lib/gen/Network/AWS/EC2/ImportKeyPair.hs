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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ImportKeyPair = ImportKeyPair
    { ikprDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , ikprKeyName :: !Text
      -- ^ The unique name for the key pair.
    , ikprPublicKeyMaterial :: !ByteString
      -- ^ The public key portion of the key pair being imported.
    } deriving (Eq, Show, Generic)

instance ToQuery ImportKeyPair

instance AWSRequest ImportKeyPair where
    type Er ImportKeyPair = EC2Error
    type Rs ImportKeyPair = ImportKeyPairResponse
    request = v2Query service GET "ImportKeyPair"

data ImportKeyPairResponse = ImportKeyPairResponse
    { ikprrsKeyFingerprint :: Maybe Text
      -- ^ The MD5 public key fingerprint as specified in section 4 of RFC4716 .
    , ikprrsKeyName :: Maybe Text
      -- ^ The specified unique key pair name.
    } deriving (Eq, Show, Generic)

instance FromXML ImportKeyPairResponse where
    fromXMLOptions = xmlOptions
