{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken completed successfully with a result (if provided). The result
-- appears in the ActivityTaskCompleted event in the workflow history. If the
-- requested task does not complete successfully, use
-- RespondActivityTaskFailed instead. If the worker finds that the task is
-- canceled through the canceled flag returned by RecordActivityTaskHeartbeat,
-- it should cancel the task, clean up and then call
-- RespondActivityTaskCanceled. A task is considered open from the time that
-- it is scheduled until it is closed. Therefore a task is reported as open
-- while a worker is processing it. A task is closed after it has been
-- specified in a call to RespondActivityTaskCompleted,
-- RespondActivityTaskCanceled, RespondActivityTaskFailed, or the task has
-- timed out. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RespondActivityTaskCompleted
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:56:15 GMT
-- X-Amz-Target: SimpleWorkflowService.RespondActivityTaskCompleted
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=M+ygHbMHSHJiVrsAQTW/BfkgHoNzLPnPD+dVywJiPXE=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 638 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAX9p3pcp3857oLXFUuwdxRU5/zmn9f40XaMF7VohAH4jOtjXpZu7GdOzEi0b3cWYHbG5b5dpdcTXHUDPVMHXiUxCgr+Nc/wUW9016W4YxJGs/jmxzPln8qLftU+SW135Q0UuKp5XRGoRTJp3tbHn2pY1vC8gDB/K69J6q668U1pd4Cd9o43//lGgOIjN0/Ihg+DO+83HNcOuVEQMM28kNMXf7yePh31M4dMKJwQaQZG13huJXDwzJOoZQz+XFuqFly+lPnCE4XvsnhfAvTsh50EtNDEtQzPCFJoUeld9g64V/FS/39PHL3M93PBUuroPyHuCwHsNC6fZ7gM/XOKmW4kKnXPoQweEUkFV/J6E6+M1reBO7nJADTrLSnajg6MY/viWsEYmMw/DS5FlquFaDIhFkLhWUWN+V2KqiKS23GYwpzgZ7fgcWHQF2NLEY3zrjam4LW/UW5VLCyM3FpVD3erCTi9IvUgslPzyVGuWNAoTmgJEWvimgwiHxJMxxc9JBDR390iMmImxVl3eeSDUWx8reQltiviadPDjyRmVhYP8",
-- "result": "customer credit card verified"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- 0976f0f4-3ff6-11e1-9a27-0760db01a4a8.
module Network.AWS.SWF
    (
    -- * Request
      RespondActivityTaskCompleted
    -- ** Request constructor
    , mkRespondActivityTaskCompleted
    -- ** Request lenses
    , ratc1TaskToken
    , ratc1Result

    -- * Response
    , RespondActivityTaskCompletedResponse
    -- ** Response constructor
    , mkRespondActivityTaskCompletedResponse
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RespondActivityTaskCompleted = RespondActivityTaskCompleted
    { _ratc1TaskToken :: !Text
    , _ratc1Result :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RespondActivityTaskCompleted' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskToken ::@ @Text@
--
-- * @Result ::@ @Maybe Text@
--
mkRespondActivityTaskCompleted :: Text -- ^ 'ratc1TaskToken'
                               -> RespondActivityTaskCompleted
mkRespondActivityTaskCompleted p1 = RespondActivityTaskCompleted
    { _ratc1TaskToken = p1
    , _ratc1Result = Nothing
    }

-- | The taskToken of the ActivityTask. The taskToken is generated by the
-- service and should be treated as an opaque value. If the task is passed to
-- another process, its taskToken must also be passed. This enables it to
-- provide its progress and respond with results.
ratc1TaskToken :: Lens' RespondActivityTaskCompleted Text
ratc1TaskToken = lens _ratc1TaskToken (\s a -> s { _ratc1TaskToken = a })

-- | The result of the activity task. It is a free form string that is
-- implementation specific.
ratc1Result :: Lens' RespondActivityTaskCompleted (Maybe Text)
ratc1Result = lens _ratc1Result (\s a -> s { _ratc1Result = a })

instance ToPath RespondActivityTaskCompleted

instance ToQuery RespondActivityTaskCompleted

instance ToHeaders RespondActivityTaskCompleted

instance ToJSON RespondActivityTaskCompleted

data RespondActivityTaskCompletedResponse = RespondActivityTaskCompletedResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RespondActivityTaskCompletedResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRespondActivityTaskCompletedResponse :: RespondActivityTaskCompletedResponse
mkRespondActivityTaskCompletedResponse = RespondActivityTaskCompletedResponse

instance AWSRequest RespondActivityTaskCompleted where
    type Sv RespondActivityTaskCompleted = SWF
    type Rs RespondActivityTaskCompleted = RespondActivityTaskCompletedResponse

    request = get
    response _ = nullaryResponse RespondActivityTaskCompletedResponse
