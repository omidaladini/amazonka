{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM configuration that contains the information required by an
-- Amazon Redshift cluster to store and use database encryption keys in a
-- Hardware Security Module (HSM). After creating the HSM configuration, you
-- can specify it as a parameter when creating a cluster. The cluster will
-- then store its encryption keys in the HSM. In addition to creating an HSM
-- configuration, you must also create an HSM client certificate. For more
-- information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.V2012_12_01.CreateHsmConfiguration
    (
    -- * Request
      CreateHsmConfiguration
    -- ** Request constructor
    , createHsmConfiguration
    -- ** Request lenses
    , chcmHsmConfigurationIdentifier
    , chcmDescription
    , chcmHsmIpAddress
    , chcmHsmPartitionName
    , chcmHsmPartitionPassword
    , chcmHsmServerPublicCertificate

    -- * Response
    , CreateHsmConfigurationResponse
    -- ** Response lenses
    , hcwHsmConfiguration
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateHsmConfiguration' request.
createHsmConfiguration :: Text -- ^ 'chcmHsmConfigurationIdentifier'
                       -> Text -- ^ 'chcmDescription'
                       -> Text -- ^ 'chcmHsmIpAddress'
                       -> Text -- ^ 'chcmHsmPartitionName'
                       -> Text -- ^ 'chcmHsmPartitionPassword'
                       -> Text -- ^ 'chcmHsmServerPublicCertificate'
                       -> CreateHsmConfiguration
createHsmConfiguration p1 p2 p3 p4 p5 p6 = CreateHsmConfiguration
    { _chcmHsmConfigurationIdentifier = p1
    , _chcmDescription = p2
    , _chcmHsmIpAddress = p3
    , _chcmHsmPartitionName = p4
    , _chcmHsmPartitionPassword = p5
    , _chcmHsmServerPublicCertificate = p6
    }

data CreateHsmConfiguration = CreateHsmConfiguration
    { _chcmHsmConfigurationIdentifier :: Text
      -- ^ The identifier to be assigned to the new Amazon Redshift HSM
      -- configuration.
    , _chcmDescription :: Text
      -- ^ A text description of the HSM configuration to be created.
    , _chcmHsmIpAddress :: Text
      -- ^ The IP address that the Amazon Redshift cluster must use to
      -- access the HSM.
    , _chcmHsmPartitionName :: Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift
      -- clusters will store their database encryption keys.
    , _chcmHsmPartitionPassword :: Text
      -- ^ The password required to access the HSM partition.
    , _chcmHsmServerPublicCertificate :: Text
      -- ^ The HSMs public certificate file. When using Cloud HSM, the file
      -- name is server.pem.
    } deriving (Show, Generic)

-- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
chcmHsmConfigurationIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmHsmConfigurationIdentifier f x =
    (\y -> x { _chcmHsmConfigurationIdentifier = y })
       <$> f (_chcmHsmConfigurationIdentifier x)
{-# INLINE chcmHsmConfigurationIdentifier #-}

-- | A text description of the HSM configuration to be created.
chcmDescription
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmDescription f x =
    (\y -> x { _chcmDescription = y })
       <$> f (_chcmDescription x)
{-# INLINE chcmDescription #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
chcmHsmIpAddress
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmHsmIpAddress f x =
    (\y -> x { _chcmHsmIpAddress = y })
       <$> f (_chcmHsmIpAddress x)
{-# INLINE chcmHsmIpAddress #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
chcmHsmPartitionName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmHsmPartitionName f x =
    (\y -> x { _chcmHsmPartitionName = y })
       <$> f (_chcmHsmPartitionName x)
{-# INLINE chcmHsmPartitionName #-}

-- | The password required to access the HSM partition.
chcmHsmPartitionPassword
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmHsmPartitionPassword f x =
    (\y -> x { _chcmHsmPartitionPassword = y })
       <$> f (_chcmHsmPartitionPassword x)
{-# INLINE chcmHsmPartitionPassword #-}

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcmHsmServerPublicCertificate
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateHsmConfiguration
    -> f CreateHsmConfiguration
chcmHsmServerPublicCertificate f x =
    (\y -> x { _chcmHsmServerPublicCertificate = y })
       <$> f (_chcmHsmServerPublicCertificate x)
{-# INLINE chcmHsmServerPublicCertificate #-}

instance ToQuery CreateHsmConfiguration where
    toQuery = genericQuery def

data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _hcwHsmConfiguration :: Maybe HsmConfiguration
      -- ^ Returns information about an HSM configuration, which is an
      -- object that describes to Amazon Redshift clusters the information
      -- they require to connect to an HSM where they can store database
      -- encryption keys.
    } deriving (Show, Generic)

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
hcwHsmConfiguration
    :: Functor f
    => (Maybe HsmConfiguration
    -> f (Maybe HsmConfiguration))
    -> CreateHsmConfigurationResponse
    -> f CreateHsmConfigurationResponse
hcwHsmConfiguration f x =
    (\y -> x { _hcwHsmConfiguration = y })
       <$> f (_hcwHsmConfiguration x)
{-# INLINE hcwHsmConfiguration #-}

instance FromXML CreateHsmConfigurationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateHsmConfiguration where
    type Sv CreateHsmConfiguration = Redshift
    type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse

    request = post "CreateHsmConfiguration"
    response _ = xmlResponse
