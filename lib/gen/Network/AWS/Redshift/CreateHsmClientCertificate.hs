{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM client certificate that an Amazon Redshift cluster will use
-- to connect to the client's HSM in order to store and retrieve the keys used
-- to encrypt the cluster databases. The command returns a public key, which
-- you must store in the HSM. After creating the HSM certificate, you must
-- create an Amazon Redshift HSM configuration that provides a cluster the
-- information needed to store and retrieve database encryption keys in the
-- HSM. For more information, go to aLinkToHSMTopic in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.CreateHsmClientCertificate where

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
createHsmClientCertificate :: Text
                           -> AWS (Either RedshiftError CreateHsmClientCertificateResponse)
createHsmClientCertificate p1 = undefined $ CreateHsmClientCertificate
    { chccmHsmClientCertificateIdentifier = p1
    }

data CreateHsmClientCertificate = CreateHsmClientCertificate
    { chccmHsmClientCertificateIdentifier :: !Text
      -- ^ The identifier to be assigned to the new HSM client certificate that the
      -- cluster will use to connect to the HSM to retrieve the database encryption
      -- keys.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateHsmClientCertificate

instance AWSRequest CreateHsmClientCertificate where
    type Er CreateHsmClientCertificate = RedshiftError
    type Rs CreateHsmClientCertificate = CreateHsmClientCertificateResponse
    request = getQuery service "CreateHsmClientCertificate"

data CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse
    { chccmrsHsmClientCertificate :: Maybe HsmClientCertificate
      -- ^ Returns information about an HSM client certificate. The certificate is
      -- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
      -- Redshift cluster to encrypt data files.
    } deriving (Eq, Show, Generic)

instance FromXML CreateHsmClientCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateHsmClientCertificateResponse"
        :| ["CreateHsmClientCertificateResult"]
