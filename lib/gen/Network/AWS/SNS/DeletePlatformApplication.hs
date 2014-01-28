{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeletePlatformApplication action deletes a platform application object
-- for one of the supported push notification services, such as APNS and GCM.
-- For more information, see Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &Action=DeletePlatformApplication &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Version=2010-03-31
-- &Signature=Mh7X%2BQo%2BGpcm5B1IpkovBaRiJCJOqvFlIOYzL62SGrg%3D
-- &Timestamp=2013-07-01T23%3A02%3A03.872Z HTTP/1.1 200 OK ...
-- 097dac18-7a77-5823-a8dd-e65476dcb037.
module Network.AWS.SNS.DeletePlatformApplication where

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

data DeletePlatformApplication = DeletePlatformApplication
    { dpaiPlatformApplicationArn :: !Text
      -- ^ PlatformApplicationArn of platform application object to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeletePlatformApplication

instance AWSRequest DeletePlatformApplication where
    type Er DeletePlatformApplication = SNSError
    type Rs DeletePlatformApplication = DeletePlatformApplicationResponse
    request = getQuery service "DeletePlatformApplication"

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Show, Generic)

instance FromXML DeletePlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeletePlatformApplicationResponse"
        :| ["DeletePlatformApplicationResult"]
