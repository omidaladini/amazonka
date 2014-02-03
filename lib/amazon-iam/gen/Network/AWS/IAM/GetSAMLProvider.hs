{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetSAMLProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the SAML provider metadocument that was uploaded when the provider
-- was created or updated. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=GetSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS 2012-05-09T16:27:11Z 2015-12-31T211:59:59Z
-- Pd9fexDssTkRgGNqs...DxptfEs== 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.GetSAMLProvider where

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
getSAMLProvider :: Text
                -> GetSAMLProvider
getSAMLProvider p1 = GetSAMLProvider
    { gsamlprSAMLProviderArn = p1
    }

data GetSAMLProvider = GetSAMLProvider
    { gsamlprSAMLProviderArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider to get information
      -- about.
    } deriving (Eq, Show, Generic)

instance ToQuery GetSAMLProvider

instance AWSRequest GetSAMLProvider where
    type Er GetSAMLProvider = IAMError
    type Rs GetSAMLProvider = GetSAMLProviderResponse
    request = getQuery service "GetSAMLProvider"

data GetSAMLProviderResponse = GetSAMLProviderResponse
    { gsamlprrsCreateDate :: Maybe UTCTime
      -- ^ The date and time when the SAML provider was created.
    , gsamlprrsSAMLMetadataDocument :: Maybe Text
      -- ^ The XML metadata document that includes information about an identity
      -- provider.
    , gsamlprrsValidUntil :: Maybe UTCTime
      -- ^ The expiration date and time for the SAML provider.
    } deriving (Eq, Show, Generic)

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetSAMLProviderResponse"
        :| ["GetSAMLProviderResult"]
