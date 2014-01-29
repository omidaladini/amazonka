{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the certificate that terminates the specified listener's SSL
-- connections. The specified certificate replaces any prior certificate that
-- was used on the same load balancer and port. For more information on
-- updating your SSL certificate, see Updating an SSL Certificate for a Load
-- Balancer in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyInternalLoadBalancer
-- &SSLCertificateId=arn:aws:iam::123456789012:server-certificate/testcert
-- &LoadBalancerPort=443 &Version=2012-06-01
-- &Action=SetLoadBalancerListenerSSLCertificate &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ElasticLoadBalancing.SetLoadBalancerListenerSSLCertificate where

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

import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields where applicable.
setLoadBalancerListenerSSLCertificate :: Text
                                      -> Int
                                      -> Text
                                      -> AWS (Either ElasticLoadBalancingError SetLoadBalancerListenerSSLCertificateResponse)
setLoadBalancerListenerSSLCertificate p1 p2 p3 = undefined $ SetLoadBalancerListenerSSLCertificate
    { slblsslciLoadBalancerName = p1
    , slblsslciLoadBalancerPort = p2
    , slblsslciSSLCertificateId = p3
    }

data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate
    { slblsslciLoadBalancerName :: !Text
      -- ^ The name of the load balancer.
    , slblsslciLoadBalancerPort :: !Int
      -- ^ The port that uses the specified SSL certificate.
    , slblsslciSSLCertificateId :: !Text
      -- ^ The Amazon Resource Number (ARN) of the SSL certificate chain to use. For
      -- more information on SSL certificates, see Managing Server Certificates in
      -- the AWS Identity and Access Management User Guide.
    } deriving (Eq, Show, Generic)

instance ToQuery SetLoadBalancerListenerSSLCertificate

instance AWSRequest SetLoadBalancerListenerSSLCertificate where
    type Er SetLoadBalancerListenerSSLCertificate = ElasticLoadBalancingError
    type Rs SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificateResponse
    request = getQuery service "SetLoadBalancerListenerSSLCertificate"

data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse
    deriving (Eq, Show, Generic)

instance FromXML SetLoadBalancerListenerSSLCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetLoadBalancerListenerSSLCertificateResponse"
        :| ["SetLoadBalancerListenerSSLCertificateResult"]
