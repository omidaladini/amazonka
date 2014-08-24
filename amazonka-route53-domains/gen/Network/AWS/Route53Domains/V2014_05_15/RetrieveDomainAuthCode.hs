{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the AuthCode for the domain. To transfer a domain to
-- another registrar, you provide this value to the new registrar.
-- RetrieveDomainAuthCode Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.RetrieveDomainAuthCode
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] { "AuthCode":"rqL3*REjYH" }.
module Network.AWS.Route53Domains.V2014_05_15.RetrieveDomainAuthCode where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.V2014_05_15.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data RetrieveDomainAuthCode = RetrieveDomainAuthCode
    { _rdacrDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    } deriving (Show, Generic)

makeLenses ''RetrieveDomainAuthCode

instance ToPath RetrieveDomainAuthCode

instance ToQuery RetrieveDomainAuthCode

instance ToHeaders RetrieveDomainAuthCode

instance ToJSON RetrieveDomainAuthCode

data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse
    { _rdacsAuthCode :: Text
      -- ^ The authorization code for the domain. Type: String.
    } deriving (Show, Generic)

makeLenses ''RetrieveDomainAuthCodeResponse

instance FromJSON RetrieveDomainAuthCodeResponse

instance AWSRequest RetrieveDomainAuthCode where
    type Sv RetrieveDomainAuthCode = Route53Domains
    type Rs RetrieveDomainAuthCode = RetrieveDomainAuthCodeResponse

    request = get
    response _ = jsonResponse