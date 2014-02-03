{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified version from the specified application. You cannot
-- delete an application version that is associated with a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=First%20Release &Operation=DeleteApplicationVersion
-- &AuthParams 58dc7339-f272-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion where

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

import Network.AWS.ElasticBeanstalk.Service
import Network.AWS.ElasticBeanstalk.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteApplicationVersion :: Text
                         -> Text
                         -> DeleteApplicationVersion
deleteApplicationVersion p1 p2 = DeleteApplicationVersion
    { davmApplicationName = p1
    , davmVersionLabel = p2
    , davmDeleteSourceBundle = Nothing
    }

data DeleteApplicationVersion = DeleteApplicationVersion
    { davmApplicationName :: !Text
      -- ^ The name of the application to delete releases from.
    , davmDeleteSourceBundle :: Maybe Bool
      -- ^ Indicates whether to delete the associated source bundle from Amazon S3:
      -- true: An attempt is made to delete the associated Amazon S3 source bundle
      -- specified at time of creation. false: No action is taken on the Amazon S3
      -- source bundle specified at time of creation. Valid Values: true | false.
    , davmVersionLabel :: !Text
      -- ^ The label of the version to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteApplicationVersion

instance AWSRequest DeleteApplicationVersion where
    type Er DeleteApplicationVersion = ElasticBeanstalkError
    type Rs DeleteApplicationVersion = DeleteApplicationVersionResponse
    request = getQuery service "DeleteApplicationVersion"

data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteApplicationVersionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteApplicationVersionResponse
