{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified server certificate. You
-- should understand the implications of changing a server certificate's path
-- or name. For more information, see Managing Server Certificates in Using
-- AWS Identity and Access Management. To change a server certificate name the
-- requester must have appropriate permissions on both the source object and
-- the target object. For example, to change the name from ProductionCert to
-- ProdCert, the entity making the request must have permission on
-- ProductionCert and ProdCert, or must have permission on all (*). For more
-- information about permissions, see Permissions and Policies.
-- https://iam.amazonaws.com/ ?Action=UpdateServerCertificate
-- &ServerCertificateName=ProdServerCert
-- &NewServerCertificateName=ProdServerCertName &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateServerCertificate where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data UpdateServerCertificate = UpdateServerCertificate
    { uscuNewPath :: Maybe Text
      -- ^ The new path for the server certificate. Include this only if you are
      -- updating the server certificate's path.
    , uscuNewServerCertificateName :: Maybe Text
      -- ^ The new name for the server certificate. Include this only if you are
      -- updating the server certificate's name.
    , uscuServerCertificateName :: !Text
      -- ^ The name of the server certificate that you want to update.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateServerCertificate

instance AWSRequest UpdateServerCertificate where
    type Er UpdateServerCertificate = IAMError
    type Rs UpdateServerCertificate = UpdateServerCertificateResponse
    request = getQuery service "UpdateServerCertificate"

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateServerCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateServerCertificateResponse"
        :| ["UpdateServerCertificateResult"]