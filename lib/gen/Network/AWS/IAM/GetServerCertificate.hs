{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified server certificate.
-- https://iam.amazonaws.com/ ?Action=GetServerCertificate
-- &ServerCertificateName=ProdServerCert &Version=2010-05-08 &AUTHPARAMS
-- ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6C2EXAMPLE -----BEGIN CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE-----
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetServerCertificate where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getServerCertificate :: Text
                     -> GetServerCertificate
getServerCertificate p1 = GetServerCertificate
    { gscrServerCertificateName = p1
    }

data GetServerCertificate = GetServerCertificate
    { gscrServerCertificateName :: !Text
      -- ^ The name of the server certificate you want to retrieve information about.
    } deriving (Eq, Show, Generic)

instance ToQuery GetServerCertificate

instance AWSRequest GetServerCertificate where
    type Er GetServerCertificate = IAMError
    type Rs GetServerCertificate = GetServerCertificateResponse
    request = getQuery service "GetServerCertificate"

data GetServerCertificateResponse = GetServerCertificateResponse
    { gscrrsServerCertificate :: ServerCertificate
      -- ^ Information about the server certificate.
    } deriving (Eq, Show, Generic)

instance FromXML GetServerCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetServerCertificateResponse"
        :| ["GetServerCertificateResult"]
