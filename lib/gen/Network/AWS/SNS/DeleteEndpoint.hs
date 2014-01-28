{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteEndpoint action, which is idempotent, deletes the endpoint from
-- SNS. For more information, see Using Amazon SNS Mobile Push Notifications.
-- POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=DeleteEndpoint
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &SignatureVersion=2 &Version=2010-03-31
-- &Signature=LIc6GI3JbNhmHBEDmSxzZp648XPe5CMeFny%2BTQFtomQ%3D
-- &Timestamp=2013-07-01T23%3A00%3A12.456Z HTTP/1.1 200 OK ...
-- c1d2b191-353c-5a5f-8969-fbdd3900afa8.
module Network.AWS.SNS.DeleteEndpoint where

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

import Network.AWS.SNS.Service
import Network.AWS.SNS.Types

data DeleteEndpoint = DeleteEndpoint
    { deiEndpointArn :: !Text
      -- ^ EndpointArn of endpoint to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteEndpoint

instance AWSRequest DeleteEndpoint where
    type Er DeleteEndpoint = SNSError
    type Rs DeleteEndpoint = DeleteEndpointResponse
    request = getQuery service "DeleteEndpoint"

data DeleteEndpointResponse = DeleteEndpointResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteEndpointResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteEndpointResponse"
        :| ["DeleteEndpointResult"]
