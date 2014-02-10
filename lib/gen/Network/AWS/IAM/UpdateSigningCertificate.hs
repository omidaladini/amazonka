{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateSigningCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the status of the specified signing certificate from active to
-- disabled, or vice versa. This action can be used to disable a user's
-- signing certificate as part of a certificate rotation work flow. If the
-- UserName field is not specified, the UserName is determined implicitly
-- based on the AWS access key ID used to sign the request. Because this
-- action works for access keys under the AWS account, this API can be used to
-- manage root credentials even if the AWS account has no associated users.
-- For information about rotating certificates, see Managing Keys and
-- Certificates in Using AWS Identity and Access Management.
-- https://iam.amazonaws.com/ ?Action=UpdateSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Status=Inactive
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateSigningCertificate where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateSigningCertificate :: Text
                         -- ^ The ID of the signing certificate you want to update.
                         -> StatusType
                         -- ^ The status you want to assign to the certificate. Active means the
                         -- certificate can be used for API calls to AWS, while Inactive means the
                         -- certificate cannot be used.
                         -> UpdateSigningCertificate
updateSigningCertificate p1 p2 = UpdateSigningCertificate
    { uscCertificateId = p1
    , uscStatus = p2
    , uscUserName = Nothing
    }

data UpdateSigningCertificate = UpdateSigningCertificate
    { uscCertificateId :: !Text
      -- ^ The ID of the signing certificate you want to update.
    , uscStatus :: !StatusType
      -- ^ The status you want to assign to the certificate. Active means the
      -- certificate can be used for API calls to AWS, while Inactive means the
      -- certificate cannot be used.
    , uscUserName :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateSigningCertificate

instance AWSRequest UpdateSigningCertificate where
    type Er UpdateSigningCertificate = IAMError
    type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse
    request  = postQuery service "UpdateSigningCertificate"
    response = responseXML

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateSigningCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateSigningCertificateResponse"