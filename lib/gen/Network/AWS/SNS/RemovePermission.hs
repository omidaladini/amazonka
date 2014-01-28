{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RemovePermission action removes a statement from a topic's access
-- control policy. http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &Label=NewPermission &Action=RemovePermission &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=N1abwRY9i7zaSQmbAlm71pPf9EEFOqNbQL1alzw2yCg%3D
-- d170b150-33a8-11df-995a-2d6fbe836cc1.
module Network.AWS.SNS.RemovePermission where

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

data RemovePermission = RemovePermission
    { rpiLabel :: !Text
      -- ^ The unique label of the statement you want to remove.
    , rpiTopicArn :: !Text
      -- ^ The ARN of the topic whose access control policy you wish to modify.
    } deriving (Eq, Show, Generic)

instance ToQuery RemovePermission

instance AWSRequest RemovePermission where
    type Er RemovePermission = SNSError
    type Rs RemovePermission = RemovePermissionResponse
    request = getQuery service "RemovePermission"

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Show, Generic)

instance FromXML RemovePermissionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RemovePermissionResponse"
        :| ["RemovePermissionResult"]
