{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified HSM client certificate.
module Network.AWS.Redshift.DeleteHsmClientCertificate where

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

data DeleteHsmClientCertificate = DeleteHsmClientCertificate
    { dhccnHsmClientCertificateIdentifier :: !Text
      -- ^ The identifier of the HSM client certificate to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteHsmClientCertificate

instance AWSRequest DeleteHsmClientCertificate where
    type Er DeleteHsmClientCertificate = RedshiftError
    type Rs DeleteHsmClientCertificate = DeleteHsmClientCertificateResponse
    request = getQuery service "DeleteHsmClientCertificate"

data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteHsmClientCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteHsmClientCertificateResponse"
        :| ["DeleteHsmClientCertificateResult"]
