{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.RegisterDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a new domain. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: You cannot use an
-- IAM policy to control domain access for this action. The name of the domain
-- being registered is available as the resource of this action. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterDomain Example POST /
-- HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Fri, 13 Jan 2012 18:42:12 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterDomain Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=tzjkF55lxAxPhzp/BRGFYQRQRq6CqrM254dTDE/EncI=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 91 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530902", "description": "music",
-- "workflowExecutionRetentionPeriodInDays": "60"} HTTP/1.1 200 OK
-- Content-Length: 0 Content-Type: application/json x-amzn-RequestId:
-- 4ec4ac3f-3e16-11e1-9b11-7182192d0b57.
module Network.AWS.SWF.V2012_01_25.RegisterDomain where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'RegisterDomain' request.
registerDomain :: Text -- ^ '_rdiName'
               -> Text -- ^ '_rdiWorkflowExecutionRetentionPeriodInDays'
               -> RegisterDomain
registerDomain p1 p2 = RegisterDomain
    { _rdiName = p1
    , _rdiWorkflowExecutionRetentionPeriodInDays = p2
    , _rdiDescription = Nothing
    }

data RegisterDomain = RegisterDomain
    { _rdiName :: Text
      -- ^ Name of the domain to register. The name must be unique. The
      -- specified string must not start or end with whitespace. It must
      -- not contain a : (colon), / (slash), | (vertical bar), or any
      -- control characters (\u0000-\u001f | \u007f - \u009f). Also, it
      -- must not contain the literal string &quot;arn&quot;.
    , _rdiWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ A duration (in days) for which the record (including the history)
      -- of workflow executions in this domain should be kept by the
      -- service. After the retention period, the workflow execution will
      -- not be available in the results of visibility calls. If you pass
      -- the value NONE then there is no expiration for workflow execution
      -- history (effectively an infinite retention period).
    , _rdiDescription :: Maybe Text
      -- ^ Textual description of the domain.
    } deriving (Show, Generic)

makeLenses ''RegisterDomain

instance ToPath RegisterDomain

instance ToQuery RegisterDomain

instance ToHeaders RegisterDomain

instance ToJSON RegisterDomain

data RegisterDomainResponse = RegisterDomainResponse
    deriving (Eq, Show, Generic)

makeLenses ''RegisterDomainResponse

instance AWSRequest RegisterDomain where
    type Sv RegisterDomain = SWF
    type Rs RegisterDomain = RegisterDomainResponse

    request = get
    response _ = nullaryResponse RegisterDomainResponse