{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a SAML provider. Deleting the provider does not update any roles
-- that reference the SAML provider as a principal in their trust policies.
-- Any attempt to assume a role that references a SAML provider that has been
-- deleted will fail. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=DeleteSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS.
module Network.AWS.IAM.DeleteSAMLProvider where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSAMLProvider :: Text
                   -> DeleteSAMLProvider
deleteSAMLProvider p1 = DeleteSAMLProvider
    { dsamlprSAMLProviderArn = p1
    }

data DeleteSAMLProvider = DeleteSAMLProvider
    { dsamlprSAMLProviderArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSAMLProvider

instance AWSRequest DeleteSAMLProvider where
    type Er DeleteSAMLProvider = IAMError
    type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse
    request = getQuery service "DeleteSAMLProvider"

data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSAMLProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteSAMLProviderResponse
