{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.DeleteEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the endpoint from Amazon SNS. This action is idempotent. For more
-- information, see Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=DeleteEndpoint
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=LIc6GI3JbNhmHBEDmSxzZp648XPe5CMeFny%2BTQFtomQ%3D
-- &amp;Timestamp=2013-07-01T23%3A00%3A12.456Z HTTP/1.1 200 OK ...
-- &lt;DeleteEndpointResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;c1d2b191-353c-5a5f-8969-fbdd3900afa8&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeleteEndpointResponse&gt;.
module Network.AWS.SNS.V2010_03_31.DeleteEndpoint
    (
    -- * Request
      DeleteEndpoint
    -- ** Request constructor
    , deleteEndpoint
    -- ** Request lenses
    , deiEndpointArn

    -- * Response
    , DeleteEndpointResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteEndpoint' request.
deleteEndpoint :: Text -- ^ 'deiEndpointArn'
               -> DeleteEndpoint
deleteEndpoint p1 = DeleteEndpoint
    { _deiEndpointArn = p1
    }

data DeleteEndpoint = DeleteEndpoint
    { _deiEndpointArn :: Text
      -- ^ EndpointArn of endpoint to delete.
    } deriving (Show, Generic)

-- | EndpointArn of endpoint to delete.
deiEndpointArn
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteEndpoint
    -> f DeleteEndpoint
deiEndpointArn f x =
    (\y -> x { _deiEndpointArn = y })
       <$> f (_deiEndpointArn x)
{-# INLINE deiEndpointArn #-}

instance ToQuery DeleteEndpoint where
    toQuery = genericQuery def

data DeleteEndpointResponse = DeleteEndpointResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteEndpoint where
    type Sv DeleteEndpoint = SNS
    type Rs DeleteEndpoint = DeleteEndpointResponse

    request = post "DeleteEndpoint"
    response _ = nullaryResponse DeleteEndpointResponse
