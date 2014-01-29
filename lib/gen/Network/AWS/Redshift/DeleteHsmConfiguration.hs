{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHsmConfiguration where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields where applicable.
deleteHsmConfiguration :: Text
                       -> AWS (Either RedshiftError DeleteHsmConfigurationResponse)
deleteHsmConfiguration p1 = undefined $ DeleteHsmConfiguration
    { dhcmHsmConfigurationIdentifier = p1
    }

data DeleteHsmConfiguration = DeleteHsmConfiguration
    { dhcmHsmConfigurationIdentifier :: !Text
      -- ^ The identifier of the Amazon Redshift HSM configuration to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteHsmConfiguration

instance AWSRequest DeleteHsmConfiguration where
    type Er DeleteHsmConfiguration = RedshiftError
    type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse
    request = getQuery service "DeleteHsmConfiguration"

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteHsmConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteHsmConfigurationResponse"
        :| ["DeleteHsmConfigurationResult"]
