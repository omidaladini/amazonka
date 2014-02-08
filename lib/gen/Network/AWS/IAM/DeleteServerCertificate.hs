{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified server certificate. If you are using a server
-- certificate with Elastic Load Balancing, deleting the certificate could
-- have implications for your application. If Elastic Load Balancing doesn't
-- detect the deletion of bound certificates, it may continue to use the
-- certificates. This could cause Elastic Load Balancing to stop accepting
-- traffic. We recommend that you remove the reference to the certificate from
-- Elastic Load Balancing before using this command to delete the certificate.
-- For more information, go to DeleteLoadBalancerListeners in the Elastic Load
-- Balancing API Reference. https://iam.amazonaws.com/
-- ?Action=DeleteServerCertificate &ServerCertificateName=ProdServerCert
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteServerCertificate where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteServerCertificate :: Text
                        -> DeleteServerCertificate
deleteServerCertificate p1 = DeleteServerCertificate
    { dscsServerCertificateName = p1
    }

data DeleteServerCertificate = DeleteServerCertificate
    { dscsServerCertificateName :: !Text
      -- ^ The name of the server certificate you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteServerCertificate

instance AWSRequest DeleteServerCertificate where
    type Er DeleteServerCertificate = IAMError
    type Rs DeleteServerCertificate = DeleteServerCertificateResponse
    request = getQuery service "DeleteServerCertificate"

data DeleteServerCertificateResponse = DeleteServerCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteServerCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteServerCertificateResponse"
