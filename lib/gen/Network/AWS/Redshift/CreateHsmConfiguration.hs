{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM configuration that contains the information required by an
-- Amazon Redshift cluster to store and retrieve database encryption keys in a
-- Hardware Storeage Module (HSM). After creating the HSM configuration, you
-- can specify it as a parameter when creating a cluster. The cluster will
-- then store its encryption keys in the HSM. Before creating an HSM
-- configuration, you must have first created an HSM client certificate. For
-- more information, go to aLinkToHSMTopic in the Amazon Redshift Management
-- Guide.
module Network.AWS.Redshift.CreateHsmConfiguration where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createHsmConfiguration :: Text
                       -> Text
                       -> Text
                       -> Text
                       -> Text
                       -> Text
                       -> CreateHsmConfiguration
createHsmConfiguration p1 p2 p3 p4 p5 p6 = undefined $ CreateHsmConfiguration
    { chcmDescription = p1
    , chcmHsmConfigurationIdentifier = p2
    , chcmHsmIpAddress = p3
    , chcmHsmPartitionName = p4
    , chcmHsmPartitionPassword = p5
    , chcmHsmServerPublicCertificate = p6
    }

data CreateHsmConfiguration = CreateHsmConfiguration
    { chcmDescription :: !Text
      -- ^ A text description of the HSM configuration to be created.
    , chcmHsmConfigurationIdentifier :: !Text
      -- ^ The identifier to be assigned to the new Amazon Redshift HSM configuration.
    , chcmHsmIpAddress :: !Text
      -- ^ The IP address that the Amazon Redshift cluster must use to access the HSM.
    , chcmHsmPartitionName :: !Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift clusters
      -- will store their database encryption keys.
    , chcmHsmPartitionPassword :: !Text
      -- ^ The password required to access the HSM partition.
    , chcmHsmServerPublicCertificate :: !Text
      -- ^ The public key used to access the HSM client certificate, which was created
      -- by calling the Amazon Redshift create HSM certificate command.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateHsmConfiguration

instance AWSRequest CreateHsmConfiguration where
    type Er CreateHsmConfiguration = RedshiftError
    type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse
    request = getQuery service "CreateHsmConfiguration"

data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { chcmrsHsmConfiguration :: Maybe HsmConfiguration
      -- ^ Returns information about an HSM configuration, which is an object that
      -- describes to Amazon Redshift clusters the information they require to
      -- connect to an HSM where they can store database encryption keys.
    } deriving (Eq, Show, Generic)

instance FromXML CreateHsmConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateHsmConfigurationResponse"
        :| ["CreateHsmConfigurationResult"]
