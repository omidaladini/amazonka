{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteSigningCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified signing certificate associated with the specified
-- user. If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because this
-- action works for access keys under the AWS account, you can use this API to
-- manage root credentials even if the AWS account has no associated users.
-- https://iam.amazonaws.com/ ?Action=DeleteSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteSigningCertificate where

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

-- | Convenience method utilising default fields where applicable.
deleteSigningCertificate :: Text
                         -> AWS (Either IAMError DeleteSigningCertificateResponse)
deleteSigningCertificate p1 = undefined $ DeleteSigningCertificate
    { dscrCertificateId = p1
    , dscrUserName = Nothing
    }

data DeleteSigningCertificate = DeleteSigningCertificate
    { dscrCertificateId :: !Text
      -- ^ ID of the signing certificate to delete.
    , dscrUserName :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSigningCertificate

instance AWSRequest DeleteSigningCertificate where
    type Er DeleteSigningCertificate = IAMError
    type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse
    request = getQuery service "DeleteSigningCertificate"

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSigningCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteSigningCertificateResponse"
        :| ["DeleteSigningCertificateResult"]
