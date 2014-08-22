{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.DescribeDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified domain including description and
-- status. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeDomain Example POST /
-- HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 03:13:33 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeDomain Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=IFJtq3M366CHqMlTpyqYqd9z0ChCoKDC5SCJBsLifu4=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 21 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530901"} HTTP/1.1 200 OK Content-Length: 137 Content-Type:
-- application/json x-amzn-RequestId: e86a6779-3f26-11e1-9a27-0760db01a4a8
-- {"configuration": {"workflowExecutionRetentionPeriodInDays": "60"},
-- "domainInfo": {"description": "music", "name": "867530901", "status":
-- "REGISTERED"} }.
module Network.AWS.SWF.V2012_01_25.DescribeDomain where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DescribeDomain = DescribeDomain
    { _ddmName :: Text
      -- ^ The name of the domain to describe.
    } deriving (Show, Generic)

makeLenses ''DescribeDomain

instance ToPath DescribeDomain

instance ToQuery DescribeDomain

instance ToHeaders DescribeDomain

instance ToJSON DescribeDomain

data DescribeDomainResponse = DescribeDomainResponse
    { _ddoConfiguration :: DomainConfiguration
      -- ^ Contains the configuration settings of a domain.
    , _ddoDomainInfo :: DomainInfo
      -- ^ Contains general information about a domain.
    } deriving (Show, Generic)

makeLenses ''DescribeDomainResponse

instance FromJSON DescribeDomainResponse

instance AWSRequest DescribeDomain where
    type Sv DescribeDomain = SWF
    type Rs DescribeDomain = DescribeDomainResponse

    request = get
    response _ = jsonResponse